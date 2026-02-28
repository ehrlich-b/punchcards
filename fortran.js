// ============================================================
// FORTRAN IV Interpreter — Phase 3 (Richer programs)
// Supports: WRITE, READ, FORMAT, STOP, END, assignments,
//           expressions, DO/CONTINUE, IF, GOTO, INTEGER/REAL,
//           DIMENSION, relational/logical operators,
//           Iw, Fw.d, Ew.d, wX, repeat counts
// ============================================================

// --- Source Preprocessor ---

function preprocessSource(cardTexts) {
    const statements = [];
    let pendingLabel = null;
    let pendingText = '';
    let pendingCardNum = 0;
    let hasPending = false;

    for (let i = 0; i < cardTexts.length; i++) {
        const raw = (cardTexts[i] || '').padEnd(80);
        const col1 = raw[0].toUpperCase();

        if (col1 === 'C' || col1 === '*') continue;
        if (raw.substring(0, 72).trim() === '') continue;

        const labelField = raw.substring(0, 5).trim();
        const contChar = raw[5];
        const stmtText = raw.substring(6, 72);

        if (contChar !== ' ' && contChar !== '0') {
            if (hasPending) pendingText += stmtText;
            continue;
        }

        if (hasPending && pendingText.trim()) {
            statements.push({ label: pendingLabel, text: pendingText, cardNum: pendingCardNum });
        }

        pendingLabel = labelField ? parseInt(labelField, 10) : null;
        pendingText = stmtText;
        pendingCardNum = i + 1;
        hasPending = true;
    }

    if (hasPending && pendingText.trim()) {
        statements.push({ label: pendingLabel, text: pendingText, cardNum: pendingCardNum });
    }

    return statements;
}

// --- Implicit Typing (I-N rule) ---

function isImplicitInteger(name) {
    const first = name[0].toUpperCase();
    return first >= 'I' && first <= 'N';
}

function defaultValue(name) {
    return isImplicitInteger(name) ? 0 : 0.0;
}

// --- Expression Tokenizer ---

function tokenizeExpr(str) {
    const tokens = [];
    let i = 0;
    while (i < str.length) {
        if (str[i] === ' ') { i++; continue; }

        // Dot-operators: .GT. .LT. .EQ. .NE. .GE. .LE. .AND. .OR. .NOT.
        if (str[i] === '.') {
            const rest = str.substring(i).toUpperCase();
            const dotOps = ['.NOT.', '.AND.', '.OR.', '.GT.', '.LT.', '.EQ.', '.NE.', '.GE.', '.LE.'];
            let matched = false;
            for (const op of dotOps) {
                if (rest.startsWith(op)) {
                    tokens.push({ type: 'OP', value: op });
                    i += op.length;
                    matched = true;
                    break;
                }
            }
            if (matched) continue;
            // Could be a decimal point in a number — fall through
            if (i + 1 < str.length && /\d/.test(str[i + 1])) {
                let num = '.';
                i++;
                while (i < str.length && /\d/.test(str[i])) { num += str[i]; i++; }
                tokens.push({ type: 'NUM', value: num });
                continue;
            }
            i++; continue;
        }

        if (str[i] === '*' && i + 1 < str.length && str[i + 1] === '*') {
            tokens.push({ type: 'OP', value: '**' }); i += 2; continue;
        }
        if ('+-*/()=,'.includes(str[i])) {
            tokens.push({ type: 'OP', value: str[i] }); i++; continue;
        }
        // Quoted string literal
        if (str[i] === "'") {
            i++;
            let text = '';
            while (i < str.length) {
                if (str[i] === "'" && i + 1 < str.length && str[i + 1] === "'") {
                    text += "'"; i += 2;
                } else if (str[i] === "'") {
                    i++; break;
                } else {
                    text += str[i]; i++;
                }
            }
            tokens.push({ type: 'STRING', value: text }); continue;
        }
        if (/\d/.test(str[i])) {
            let num = '';
            while (i < str.length && /\d/.test(str[i])) { num += str[i]; i++; }
            // Check if dot is a decimal point or start of a dot-operator
            if (i < str.length && str[i] === '.') {
                const afterDot = str.substring(i).toUpperCase();
                const isDotOp = ['.NOT.', '.AND.', '.OR.', '.GT.', '.LT.', '.EQ.', '.NE.', '.GE.', '.LE.'].some(op => afterDot.startsWith(op));
                if (!isDotOp) {
                    num += str[i]; i++;
                    while (i < str.length && /\d/.test(str[i])) { num += str[i]; i++; }
                }
            }
            tokens.push({ type: 'NUM', value: num }); continue;
        }
        if (/[A-Za-z]/.test(str[i])) {
            let name = '';
            while (i < str.length && /[A-Za-z0-9]/.test(str[i])) { name += str[i]; i++; }
            tokens.push({ type: 'ID', value: name.toUpperCase() }); continue;
        }
        i++;
    }
    return tokens;
}

// --- Expression Parser (recursive descent) ---
// Precedence (low to high): .OR., .AND., .NOT., relational, +/-, *//, **, unary

function parseExpr(tokens, pos) {
    return parseOr(tokens, pos);
}

function parseOr(tokens, pos) {
    let [left, p] = parseAnd(tokens, pos);
    while (p < tokens.length && tokens[p].type === 'OP' && tokens[p].value === '.OR.') {
        p++;
        let right;
        [right, p] = parseAnd(tokens, p);
        left = { type: 'LOGOP', op: '.OR.', left, right, isReal: false };
    }
    return [left, p];
}

function parseAnd(tokens, pos) {
    let [left, p] = parseNot(tokens, pos);
    while (p < tokens.length && tokens[p].type === 'OP' && tokens[p].value === '.AND.') {
        p++;
        let right;
        [right, p] = parseNot(tokens, p);
        left = { type: 'LOGOP', op: '.AND.', left, right, isReal: false };
    }
    return [left, p];
}

function parseNot(tokens, pos) {
    if (pos < tokens.length && tokens[pos].type === 'OP' && tokens[pos].value === '.NOT.') {
        pos++;
        let [operand, p] = parseNot(tokens, pos);
        return [{ type: 'LOGOP', op: '.NOT.', operand, isReal: false }, p];
    }
    return parseRelational(tokens, pos);
}

const REL_OPS = new Set(['.GT.', '.LT.', '.EQ.', '.NE.', '.GE.', '.LE.']);

function parseRelational(tokens, pos) {
    let [left, p] = parseAddSub(tokens, pos);
    if (p < tokens.length && tokens[p].type === 'OP' && REL_OPS.has(tokens[p].value)) {
        const op = tokens[p].value; p++;
        let right;
        [right, p] = parseAddSub(tokens, p);
        left = { type: 'RELOP', op, left, right, isReal: false };
    }
    return [left, p];
}

function parseAddSub(tokens, pos) {
    let [left, p] = parseMulDiv(tokens, pos);
    while (p < tokens.length && tokens[p].type === 'OP' && (tokens[p].value === '+' || tokens[p].value === '-')) {
        const op = tokens[p].value; p++;
        let right;
        [right, p] = parseMulDiv(tokens, p);
        left = { type: 'BINOP', op, left, right, isReal: left.isReal || right.isReal };
    }
    return [left, p];
}

function parseMulDiv(tokens, pos) {
    let [left, p] = parsePower(tokens, pos);
    while (p < tokens.length && tokens[p].type === 'OP' && (tokens[p].value === '*' || tokens[p].value === '/')) {
        const op = tokens[p].value; p++;
        let right;
        [right, p] = parsePower(tokens, p);
        left = { type: 'BINOP', op, left, right, isReal: left.isReal || right.isReal };
    }
    return [left, p];
}

function parsePower(tokens, pos) {
    let [base, p] = parseUnary(tokens, pos);
    if (p < tokens.length && tokens[p].type === 'OP' && tokens[p].value === '**') {
        p++;
        let exp;
        [exp, p] = parsePower(tokens, p);
        base = { type: 'BINOP', op: '**', left: base, right: exp, isReal: base.isReal || exp.isReal };
    }
    return [base, p];
}

function parseUnary(tokens, pos) {
    if (pos < tokens.length && tokens[pos].type === 'OP' && (tokens[pos].value === '+' || tokens[pos].value === '-')) {
        const op = tokens[pos].value; pos++;
        let [operand, p] = parsePrimary(tokens, pos);
        if (op === '-') operand = { type: 'UNARY', op: '-', operand, isReal: operand.isReal };
        return [operand, p];
    }
    return parsePrimary(tokens, pos);
}

function parsePrimary(tokens, pos) {
    if (pos >= tokens.length) return [{ type: 'NUM_LIT', value: 0 }, pos];
    const tok = tokens[pos];
    if (tok.type === 'NUM') {
        const isReal = tok.value.includes('.');
        return [{ type: 'NUM_LIT', value: isReal ? parseFloat(tok.value) : parseInt(tok.value, 10), isReal }, pos + 1];
    }
    if (tok.type === 'STRING') {
        return [{ type: 'STRING_LIT', value: tok.value, isReal: false }, pos + 1];
    }
    if (tok.type === 'ID') {
        // Check for array reference: NAME(subscripts)
        if (pos + 1 < tokens.length && tokens[pos + 1].type === 'OP' && tokens[pos + 1].value === '(') {
            const name = tok.value;
            let p = pos + 2;
            const subscripts = [];
            while (p < tokens.length) {
                let sub;
                [sub, p] = parseExpr(tokens, p);
                subscripts.push(sub);
                if (p < tokens.length && tokens[p].type === 'OP' && tokens[p].value === ',') { p++; continue; }
                break;
            }
            if (p < tokens.length && tokens[p].type === 'OP' && tokens[p].value === ')') p++;
            return [{ type: 'ARRAY_REF', name, subscripts, isReal: !isImplicitInteger(name) }, p];
        }
        return [{ type: 'VAR', name: tok.value, isReal: !isImplicitInteger(tok.value) }, pos + 1];
    }
    if (tok.type === 'OP' && tok.value === '(') {
        pos++;
        let [expr, p] = parseExpr(tokens, pos);
        if (p < tokens.length && tokens[p].type === 'OP' && tokens[p].value === ')') p++;
        return [expr, p];
    }
    return [{ type: 'NUM_LIT', value: 0, isReal: false }, pos + 1];
}

function parseExprList(str) {
    const tokens = tokenizeExpr(str);
    const exprs = [];
    let pos = 0;
    while (pos < tokens.length) {
        let expr;
        [expr, pos] = parseExpr(tokens, pos);
        exprs.push(expr);
        if (pos < tokens.length && tokens[pos].type === 'OP' && tokens[pos].value === ',') pos++;
    }
    return exprs;
}

// --- Expression Evaluator ---

function evalExpr(node, vars, arrays) {
    switch (node.type) {
        case 'NUM_LIT': return node.value;
        case 'STRING_LIT': return node.value;
        case 'VAR': return vars[node.name] !== undefined ? vars[node.name] : defaultValue(node.name);
        case 'UNARY': return -evalExpr(node.operand, vars, arrays);
        case 'BINOP': {
            const l = evalExpr(node.left, vars, arrays);
            const r = evalExpr(node.right, vars, arrays);
            const bothInt = !node.isReal && Number.isInteger(l) && Number.isInteger(r);
            switch (node.op) {
                case '+': return bothInt ? (l + r) | 0 : l + r;
                case '-': return bothInt ? (l - r) | 0 : l - r;
                case '*': return bothInt ? (l * r) | 0 : l * r;
                case '/': return bothInt ? Math.trunc(l / r) : l / r;
                case '**': return bothInt ? Math.pow(l, r) | 0 : Math.pow(l, r);
            }
            break;
        }
        case 'RELOP': {
            const l = evalExpr(node.left, vars, arrays);
            const r = evalExpr(node.right, vars, arrays);
            let result;
            switch (node.op) {
                case '.GT.': result = l > r; break;
                case '.LT.': result = l < r; break;
                case '.EQ.': result = l === r; break;
                case '.NE.': result = l !== r; break;
                case '.GE.': result = l >= r; break;
                case '.LE.': result = l <= r; break;
            }
            return result ? 1 : 0;
        }
        case 'LOGOP': {
            if (node.op === '.NOT.') return evalExpr(node.operand, vars, arrays) === 0 ? 1 : 0;
            const l = evalExpr(node.left, vars, arrays);
            const r = evalExpr(node.right, vars, arrays);
            if (node.op === '.AND.') return (l !== 0 && r !== 0) ? 1 : 0;
            if (node.op === '.OR.') return (l !== 0 || r !== 0) ? 1 : 0;
            break;
        }
        case 'ARRAY_REF': {
            if (!arrays) return 0;
            const arr = arrays[node.name];
            if (!arr) return 0;
            const idx = node.subscripts.map(s => evalExpr(s, vars, arrays));
            const key = idx.join(',');
            return arr.data[key] !== undefined ? arr.data[key] : defaultValue(node.name);
        }
    }
    return 0;
}

function coerceToType(name, value) {
    if (typeof value === 'string') return value;
    if (isImplicitInteger(name)) return Math.trunc(value);
    return typeof value === 'number' ? value : parseFloat(value);
}

// --- Parser ---

function parseProgram(statements) {
    const ast = [];
    const formats = {};

    for (const stmt of statements) {
        const node = parseStatement(stmt);
        if (!node) continue;
        node.label = stmt.label;
        node.cardNum = stmt.cardNum;
        ast.push(node);
        if (node.type === 'FORMAT' && stmt.label != null) {
            formats[stmt.label] = node;
        }
    }

    return { ast, formats };
}

function parseStatement(stmt) {
    const compressed = stmt.text.replace(/\s/g, '').toUpperCase();

    if (compressed.startsWith('WRITE(')) return parseWrite(compressed, stmt);
    if (compressed.startsWith('READ(')) return parseRead(compressed, stmt);
    if (compressed.startsWith('FORMAT(')) return parseFormat(stmt.text, stmt);
    if (compressed === 'STOP' || compressed.startsWith('STOP')) return { type: 'STOP' };
    if (compressed === 'END') return { type: 'END' };
    if (compressed === 'CONTINUE') return { type: 'CONTINUE' };
    if (compressed.startsWith('GOTO')) return parseGoto(compressed);
    if (compressed.startsWith('DO')) return parseDo(compressed);
    if (compressed.startsWith('IF(')) return parseIf(stmt.text, stmt);
    if (compressed.startsWith('INTEGER')) return parseTypeDecl(compressed, 'INTEGER');
    if (compressed.startsWith('REAL')) return parseTypeDecl(compressed, 'REAL');
    if (compressed.startsWith('DIMENSION')) return parseDimension(compressed);

    // Assignment: VAR = expr  or  VAR(subscripts) = expr
    const arrAssign = compressed.match(/^([A-Z][A-Z0-9]*)\(([^)]+)\)=(.*)/);
    if (arrAssign) return parseArrayAssignment(arrAssign[1], arrAssign[2], arrAssign[3]);
    const eqMatch = compressed.match(/^([A-Z][A-Z0-9]*)=(.*)/);
    if (eqMatch) return parseAssignment(eqMatch[1], eqMatch[2]);

    return { type: 'UNKNOWN', text: stmt.text.trim() };
}

function parseAssignment(varName, exprStr) {
    const tokens = tokenizeExpr(exprStr);
    const [expr] = parseExpr(tokens, 0);
    return { type: 'ASSIGN', varName, expr };
}

function parseArrayAssignment(name, subsStr, exprStr) {
    const subscripts = parseExprList(subsStr);
    const tokens = tokenizeExpr(exprStr);
    const [expr] = parseExpr(tokens, 0);
    return { type: 'ARRAY_ASSIGN', name, subscripts, expr };
}

function parseGoto(compressed) {
    const m = compressed.match(/^GOTO(\d+)/);
    if (!m) return { type: 'UNKNOWN', text: compressed };
    return { type: 'GOTO', target: parseInt(m[1], 10) };
}

function parseDo(compressed) {
    const m = compressed.match(/^DO(\d+)([A-Z][A-Z0-9]*)=(.+)/);
    if (!m) return { type: 'UNKNOWN', text: compressed };
    const label = parseInt(m[1], 10);
    const varName = m[2];
    const parts = m[3].split(',');
    const startTokens = tokenizeExpr(parts[0]);
    const endTokens = tokenizeExpr(parts[1]);
    const [startExpr] = parseExpr(startTokens, 0);
    const [endExpr] = parseExpr(endTokens, 0);
    let stepExpr = { type: 'NUM_LIT', value: 1 };
    if (parts.length > 2) {
        const stepTokens = tokenizeExpr(parts[2]);
        [stepExpr] = parseExpr(stepTokens, 0);
    }
    return { type: 'DO', targetLabel: label, varName, start: startExpr, end: endExpr, step: stepExpr };
}

function parseIf(rawText, stmt) {
    const compressed = rawText.replace(/\s/g, '');
    const ifStart = compressed.toUpperCase().indexOf('IF(');
    if (ifStart === -1) return { type: 'UNKNOWN', text: stmt.text.trim() };
    let depth = 0, condEnd = -1;
    for (let i = ifStart + 2; i < compressed.length; i++) {
        if (compressed[i] === '(') depth++;
        else if (compressed[i] === ')') { depth--; if (depth === 0) { condEnd = i; break; } }
    }
    if (condEnd === -1) return { type: 'UNKNOWN', text: stmt.text.trim() };
    const condStr = compressed.substring(ifStart + 3, condEnd);
    const bodyStr = compressed.substring(condEnd + 1);
    const condTokens = tokenizeExpr(condStr);
    const [condExpr] = parseExpr(condTokens, 0);

    const arithMatch = bodyStr.match(/^(\d+),(\d+),(\d+)$/);
    if (arithMatch) {
        return {
            type: 'ARITH_IF',
            condition: condExpr,
            negLabel: parseInt(arithMatch[1], 10),
            zeroLabel: parseInt(arithMatch[2], 10),
            posLabel: parseInt(arithMatch[3], 10),
        };
    }

    const bodyNode = parseStatement({ text: bodyStr, label: null, cardNum: stmt.cardNum });
    return { type: 'IF', condition: condExpr, body: bodyNode };
}

function parseTypeDecl(compressed, typeName) {
    const varsPart = compressed.substring(typeName.length);
    const varNames = varsPart.split(',').map(v => v.trim()).filter(v => v);
    return { type: 'TYPE_DECL', declType: typeName, varNames };
}

function parseDimension(compressed) {
    // DIMENSION A(10), B(5,5), ...
    const rest = compressed.substring(9); // after 'DIMENSION'
    const dims = [];
    const re = /([A-Z][A-Z0-9]*)\(([^)]+)\)/g;
    let m;
    while ((m = re.exec(rest)) !== null) {
        const sizes = m[2].split(',').map(s => parseInt(s.trim(), 10));
        dims.push({ name: m[1], sizes });
    }
    return { type: 'DIMENSION', dims };
}

function parseWrite(compressed, stmt) {
    const closeParen = compressed.indexOf(')', 6);
    if (closeParen === -1) return { type: 'UNKNOWN', text: stmt.text.trim() };
    const inner = compressed.substring(6, closeParen);
    const parts = inner.split(',');
    const unit = parseInt(parts[0], 10);
    const formatLabel = parts.length > 1 ? parseInt(parts[1], 10) : null;
    const rest = compressed.substring(closeParen + 1).trim();
    const ioList = rest ? parseIOList(rest) : [];
    return { type: 'WRITE', unit, formatLabel, ioList };
}

function parseIOList(str) {
    const tokens = tokenizeExpr(str);
    const items = [];
    let pos = 0;
    while (pos < tokens.length) {
        let item;
        [item, pos] = parseIOItem(tokens, pos);
        items.push(item);
        if (pos < tokens.length && tokens[pos].type === 'OP' && tokens[pos].value === ',') pos++;
    }
    return items;
}

function parseIOItem(tokens, pos) {
    if (pos < tokens.length && tokens[pos].type === 'OP' && tokens[pos].value === '(') {
        // Scan for '=' at depth 1 to detect implied DO
        let depth = 1;
        let equalsPos = -1;
        for (let i = pos + 1; i < tokens.length; i++) {
            if (tokens[i].type === 'OP' && tokens[i].value === '(') depth++;
            else if (tokens[i].type === 'OP' && tokens[i].value === ')') {
                depth--;
                if (depth === 0) break;
            } else if (tokens[i].type === 'OP' && tokens[i].value === '=' && depth === 1) {
                equalsPos = i;
            }
        }
        if (equalsPos >= 2 && tokens[equalsPos - 1].type === 'ID' &&
            tokens[equalsPos - 2].type === 'OP' && tokens[equalsPos - 2].value === ',') {
            return parseImpliedDo(tokens, pos, equalsPos);
        }
    }
    let expr;
    [expr, pos] = parseExpr(tokens, pos);
    return [{ type: 'IO_EXPR', expr }, pos];
}

function parseImpliedDo(tokens, pos, equalsPos) {
    pos++; // skip opening '('
    const commaPos = equalsPos - 2;
    const items = [];
    while (pos < commaPos) {
        let item;
        [item, pos] = parseIOItem(tokens, pos);
        items.push(item);
        if (pos < commaPos && tokens[pos].type === 'OP' && tokens[pos].value === ',') pos++;
    }
    pos = equalsPos - 1; // DO variable
    const varName = tokens[pos].value;
    pos = equalsPos + 1; // skip '='
    let start;
    [start, pos] = parseExpr(tokens, pos);
    if (pos < tokens.length && tokens[pos].type === 'OP' && tokens[pos].value === ',') pos++;
    let end;
    [end, pos] = parseExpr(tokens, pos);
    let step = { type: 'NUM_LIT', value: 1 };
    if (pos < tokens.length && tokens[pos].type === 'OP' && tokens[pos].value === ',') {
        pos++;
        [step, pos] = parseExpr(tokens, pos);
    }
    if (pos < tokens.length && tokens[pos].type === 'OP' && tokens[pos].value === ')') pos++;
    return [{ type: 'IMPLIED_DO', items, varName, start, end, step }, pos];
}

function parseRead(compressed, stmt) {
    const closeParen = compressed.indexOf(')', 5);
    if (closeParen === -1) return { type: 'UNKNOWN', text: stmt.text.trim() };
    const inner = compressed.substring(5, closeParen);
    const parts = inner.split(',');
    const unit = parseInt(parts[0], 10);
    const formatLabel = parts.length > 1 ? parseInt(parts[1], 10) : null;
    const rest = compressed.substring(closeParen + 1).trim();
    // Parse variable names for READ targets
    const varNames = rest ? rest.split(',').map(v => v.trim()) : [];
    return { type: 'READ', unit, formatLabel, varNames };
}

// FORMAT must be parsed from original text to preserve spaces in Hollerith constants
function parseFormat(rawText, stmt) {
    const openParen = rawText.indexOf('(');
    if (openParen === -1) return { type: 'FORMAT', descriptors: [] };

    let depth = 0, closeParen = -1;
    for (let i = openParen; i < rawText.length; i++) {
        if (rawText[i] === '(') depth++;
        else if (rawText[i] === ')') { depth--; if (depth === 0) { closeParen = i; break; } }
    }
    if (closeParen === -1) closeParen = rawText.length;

    const body = rawText.substring(openParen + 1, closeParen);
    return { type: 'FORMAT', descriptors: parseFormatDescriptors(body) };
}

function parseFormatDescriptors(str) {
    const descriptors = [];
    let i = 0;

    while (i < str.length) {
        if (str[i] === ' ' || str[i] === ',') { i++; continue; }

        // Number prefix
        if (/\d/.test(str[i])) {
            let numStr = '';
            while (i < str.length && /\d/.test(str[i])) { numStr += str[i]; i++; }
            const num = parseInt(numStr, 10);
            if (i >= str.length) continue;
            const ch = str[i].toUpperCase();
            if (ch === 'H') {
                i++;
                const text = str.substring(i, i + num);
                i += num;
                descriptors.push({ type: 'H', text });
                continue;
            }
            if (ch === 'X') {
                i++;
                descriptors.push({ type: 'X', width: num });
                continue;
            }
            if (ch === 'I') {
                i++;
                let w = '';
                while (i < str.length && /\d/.test(str[i])) { w += str[i]; i++; }
                const width = w ? parseInt(w, 10) : 10;
                // Repeat count: num is repeat, width is the I descriptor width
                for (let r = 0; r < num; r++) descriptors.push({ type: 'I', width });
                continue;
            }
            if (ch === 'F') {
                i++;
                let w = '';
                while (i < str.length && /\d/.test(str[i])) { w += str[i]; i++; }
                let d = 0;
                if (i < str.length && str[i] === '.') {
                    i++;
                    let dStr = '';
                    while (i < str.length && /\d/.test(str[i])) { dStr += str[i]; i++; }
                    d = dStr ? parseInt(dStr, 10) : 0;
                }
                const width = w ? parseInt(w, 10) : 10;
                for (let r = 0; r < num; r++) descriptors.push({ type: 'F', width, decimals: d });
                continue;
            }
            if (ch === 'E') {
                i++;
                let w = '';
                while (i < str.length && /\d/.test(str[i])) { w += str[i]; i++; }
                let d = 0;
                if (i < str.length && str[i] === '.') {
                    i++;
                    let dStr = '';
                    while (i < str.length && /\d/.test(str[i])) { dStr += str[i]; i++; }
                    d = dStr ? parseInt(dStr, 10) : 0;
                }
                const width = w ? parseInt(w, 10) : 14;
                for (let r = 0; r < num; r++) descriptors.push({ type: 'E', width, decimals: d });
                continue;
            }
            if (ch === 'A') {
                i++;
                let w = '';
                while (i < str.length && /\d/.test(str[i])) { w += str[i]; i++; }
                const width = w ? parseInt(w, 10) : null;
                for (let r = 0; r < num; r++) descriptors.push({ type: 'A', width });
                continue;
            }
            // Bare number not followed by a known descriptor — skip
            continue;
        }

        // Quoted string
        if (str[i] === "'") {
            i++;
            let text = '';
            while (i < str.length) {
                if (str[i] === "'" && i + 1 < str.length && str[i + 1] === "'") {
                    text += "'"; i += 2;
                } else if (str[i] === "'") {
                    i++; break;
                } else {
                    text += str[i]; i++;
                }
            }
            descriptors.push({ type: 'STRING', text });
            continue;
        }

        if (str[i] === '/') {
            descriptors.push({ type: 'SLASH' });
            i++;
            continue;
        }

        // Bare descriptors without repeat count
        const uch = str[i].toUpperCase();
        if (uch === 'I') {
            i++;
            let w = '';
            while (i < str.length && /\d/.test(str[i])) { w += str[i]; i++; }
            descriptors.push({ type: 'I', width: w ? parseInt(w, 10) : 10 });
            continue;
        }
        if (uch === 'F') {
            i++;
            let w = '';
            while (i < str.length && /\d/.test(str[i])) { w += str[i]; i++; }
            let d = 0;
            if (i < str.length && str[i] === '.') {
                i++;
                let dStr = '';
                while (i < str.length && /\d/.test(str[i])) { dStr += str[i]; i++; }
                d = dStr ? parseInt(dStr, 10) : 0;
            }
            descriptors.push({ type: 'F', width: w ? parseInt(w, 10) : 10, decimals: d });
            continue;
        }
        if (uch === 'E') {
            i++;
            let w = '';
            while (i < str.length && /\d/.test(str[i])) { w += str[i]; i++; }
            let d = 0;
            if (i < str.length && str[i] === '.') {
                i++;
                let dStr = '';
                while (i < str.length && /\d/.test(str[i])) { dStr += str[i]; i++; }
                d = dStr ? parseInt(dStr, 10) : 0;
            }
            descriptors.push({ type: 'E', width: w ? parseInt(w, 10) : 14, decimals: d });
            continue;
        }
        if (uch === 'A') {
            i++;
            let width = '';
            while (i < str.length && /\d/.test(str[i])) { width += str[i]; i++; }
            descriptors.push({ type: 'A', width: width ? parseInt(width, 10) : null });
            continue;
        }

        i++;
    }

    return descriptors;
}

// --- Format output helpers ---

function formatInteger(value, width) {
    const s = String(Math.trunc(value));
    return s.length >= width ? s : s.padStart(width, ' ');
}

function formatReal(value, width, decimals) {
    const s = Number(value).toFixed(decimals);
    return s.length >= width ? s : s.padStart(width, ' ');
}

function formatExponential(value, width, decimals) {
    const s = Number(value).toExponential(decimals).toUpperCase().replace('E+', 'E+').replace('E-', 'E-');
    return s.length >= width ? s : s.padStart(width, ' ');
}

function expandIOList(items, vars, arrays) {
    const values = [];
    for (const item of items) {
        if (item.type === 'IO_EXPR') {
            values.push(evalExpr(item.expr, vars, arrays));
        } else if (item.type === 'IMPLIED_DO') {
            const startVal = evalExpr(item.start, vars, arrays);
            const endVal = evalExpr(item.end, vars, arrays);
            const stepVal = evalExpr(item.step, vars, arrays);
            for (let v = startVal; stepVal > 0 ? v <= endVal : v >= endVal; v += stepVal) {
                vars[item.varName] = v;
                values.push(...expandIOList(item.items, vars, arrays));
            }
        }
    }
    return values;
}

function executeWrite(node, formats, vars, arrays, output) {
    if (node.unit !== 6) return null;
    const fmt = formats[node.formatLabel];
    if (!fmt) return 'FORMAT label ' + node.formatLabel + ' not found';
    const ioValues = expandIOList(node.ioList, vars, arrays);
    let line = '';
    let ioIdx = 0;
    for (const desc of fmt.descriptors) {
        if (desc.type === 'H' || desc.type === 'STRING') {
            line += desc.text;
        } else if (desc.type === 'SLASH') {
            output.push(line);
            line = '';
        } else if (desc.type === 'X') {
            line += ' '.repeat(desc.width);
        } else if (desc.type === 'I') {
            const val = ioIdx < ioValues.length ? ioValues[ioIdx++] : 0;
            line += formatInteger(val, desc.width);
        } else if (desc.type === 'F') {
            const val = ioIdx < ioValues.length ? ioValues[ioIdx++] : 0.0;
            line += formatReal(val, desc.width, desc.decimals);
        } else if (desc.type === 'E') {
            const val = ioIdx < ioValues.length ? ioValues[ioIdx++] : 0.0;
            line += formatExponential(val, desc.width, desc.decimals);
        } else if (desc.type === 'A') {
            const val = ioIdx < ioValues.length ? ioValues[ioIdx++] : ' ';
            const s = String(val);
            if (desc.width) {
                line += s.length >= desc.width ? s.substring(0, desc.width) : s.padStart(desc.width);
            } else {
                line += s;
            }
        }
    }
    output.push(line);
    return null;
}

// --- Interpreter (generator-based for stepping debugger) ---

function executeIfBody(node, body, vars, arrays, formats, output, labelIndex) {
    switch (body.type) {
        case 'GOTO': {
            const target = labelIndex[body.target];
            if (target === undefined) return { error: 'Card ' + node.cardNum + ': GOTO label ' + body.target + ' not found' };
            return { jump: target };
        }
        case 'ASSIGN':
            vars[body.varName] = coerceToType(body.varName, evalExpr(body.expr, vars, arrays));
            return {};
        case 'ARRAY_ASSIGN': {
            const idx = body.subscripts.map(s => evalExpr(s, vars, arrays));
            const key = idx.join(',');
            if (!arrays[body.name]) arrays[body.name] = { data: {} };
            arrays[body.name].data[key] = coerceToType(body.name, evalExpr(body.expr, vars, arrays));
            return {};
        }
        case 'STOP':
            return { stop: true };
        case 'WRITE': {
            const err = executeWrite(body, formats, vars, arrays, output);
            if (err) return { error: 'Card ' + node.cardNum + ': ' + err };
            return {};
        }
    }
    return {};
}

function* executeProgram(cardTexts) {
    const output = [];
    const vars = {};
    const arrays = {};

    const statements = preprocessSource(cardTexts);
    if (statements.length === 0) {
        return { pc: 0, cardNum: 0, nodeType: null, output: [], variables: {}, done: true, error: 'No executable statements found' };
    }

    let program;
    try {
        program = parseProgram(statements);
    } catch (e) {
        return { pc: 0, cardNum: 0, nodeType: null, output: [], variables: {}, done: true, error: 'Parse error: ' + e.message };
    }

    const { ast, formats } = program;

    const labelIndex = {};
    for (let i = 0; i < ast.length; i++) {
        if (ast[i].label != null) labelIndex[ast[i].label] = i;
    }

    const doStack = [];
    const MAX_STEPS = 1000000;
    let stepCount = 0;

    let pc = 0;
    while (pc < ast.length) {
        if (++stepCount > MAX_STEPS) {
            return { pc, cardNum: ast[pc].cardNum, nodeType: ast[pc].type, output: [...output], variables: { ...vars }, done: true, error: 'Execution limit exceeded (possible infinite loop)' };
        }

        const node = ast[pc];
        yield { pc, cardNum: node.cardNum, nodeType: node.type, output: [...output], variables: { ...vars }, done: false, error: null };

        switch (node.type) {
            case 'WRITE': {
                const err = executeWrite(node, formats, vars, arrays, output);
                if (err) return { pc, cardNum: node.cardNum, nodeType: node.type, output: [...output], variables: { ...vars }, done: true, error: 'Card ' + node.cardNum + ': ' + err };
                pc++; break;
            }

            case 'READ': {
                // READ from unit 5 (card reader) — prompt user
                if (node.unit === 5) {
                    const input = prompt('CARD READER INPUT:');
                    if (input !== null) {
                        // Parse space/comma separated values
                        const values = input.trim().split(/[\s,]+/);
                        for (let vi = 0; vi < node.varNames.length && vi < values.length; vi++) {
                            const name = node.varNames[vi];
                            const val = isImplicitInteger(name) ? parseInt(values[vi], 10) : parseFloat(values[vi]);
                            vars[name] = isNaN(val) ? 0 : val;
                        }
                    }
                }
                pc++; break;
            }

            case 'FORMAT':
                pc++; break;

            case 'ASSIGN':
                vars[node.varName] = coerceToType(node.varName, evalExpr(node.expr, vars, arrays));
                pc++; break;

            case 'ARRAY_ASSIGN': {
                const idx = node.subscripts.map(s => evalExpr(s, vars, arrays));
                const key = idx.join(',');
                if (!arrays[node.name]) arrays[node.name] = { data: {} };
                arrays[node.name].data[key] = coerceToType(node.name, evalExpr(node.expr, vars, arrays));
                pc++; break;
            }

            case 'TYPE_DECL':
                pc++; break;

            case 'DIMENSION':
                for (const dim of node.dims) {
                    if (!arrays[dim.name]) arrays[dim.name] = { sizes: dim.sizes, data: {} };
                    else arrays[dim.name].sizes = dim.sizes;
                }
                pc++; break;

            case 'GOTO': {
                const target = labelIndex[node.target];
                if (target === undefined) return { pc, cardNum: node.cardNum, nodeType: node.type, output: [...output], variables: { ...vars }, done: true, error: 'Card ' + node.cardNum + ': GOTO label ' + node.target + ' not found' };
                while (doStack.length > 0) {
                    const loop = doStack[doStack.length - 1];
                    if (target < loop.loopStart || target > loop.targetIndex) doStack.pop();
                    else break;
                }
                pc = target; break;
            }

            case 'DO': {
                const startVal = evalExpr(node.start, vars, arrays);
                const endVal = evalExpr(node.end, vars, arrays);
                const stepVal = evalExpr(node.step, vars, arrays);
                vars[node.varName] = coerceToType(node.varName, startVal);
                const targetIdx = labelIndex[node.targetLabel];
                if (targetIdx === undefined) return { pc, cardNum: node.cardNum, nodeType: node.type, output: [...output], variables: { ...vars }, done: true, error: 'Card ' + node.cardNum + ': DO target label ' + node.targetLabel + ' not found' };
                doStack.push({ varName: node.varName, end: endVal, step: stepVal, targetIndex: targetIdx, loopStart: pc + 1 });
                pc++; break;
            }

            case 'CONTINUE': {
                if (doStack.length > 0) {
                    const loop = doStack[doStack.length - 1];
                    if (labelIndex[ast[pc].label] === pc && loop.targetIndex === pc) {
                        vars[loop.varName] += loop.step;
                        const done = loop.step > 0 ? vars[loop.varName] > loop.end : vars[loop.varName] < loop.end;
                        if (!done) { pc = loop.loopStart; break; }
                        doStack.pop();
                    }
                }
                pc++; break;
            }

            case 'ARITH_IF': {
                const aval = evalExpr(node.condition, vars, arrays);
                const targetLabel = aval < 0 ? node.negLabel : aval === 0 ? node.zeroLabel : node.posLabel;
                const aidx = labelIndex[targetLabel];
                if (aidx === undefined) return { pc, cardNum: node.cardNum, nodeType: 'ARITH_IF', output: [...output], variables: { ...vars }, done: true, error: 'Card ' + node.cardNum + ': IF target label ' + targetLabel + ' not found' };
                while (doStack.length > 0) {
                    const loop = doStack[doStack.length - 1];
                    if (aidx < loop.loopStart || aidx > loop.targetIndex) doStack.pop();
                    else break;
                }
                pc = aidx; break;
            }

            case 'IF': {
                const condVal = evalExpr(node.condition, vars, arrays);
                if (condVal !== 0) {
                    const result = executeIfBody(node, node.body, vars, arrays, formats, output, labelIndex);
                    if (result.error) return { pc, cardNum: node.cardNum, nodeType: 'IF', output: [...output], variables: { ...vars }, done: true, error: result.error };
                    if (result.stop) return { pc, cardNum: node.cardNum, nodeType: 'STOP', output: [...output], variables: { ...vars }, done: true, error: null };
                    if (result.jump !== undefined) {
                        while (doStack.length > 0) {
                            const loop = doStack[doStack.length - 1];
                            if (result.jump < loop.loopStart || result.jump > loop.targetIndex) doStack.pop();
                            else break;
                        }
                        pc = result.jump; break;
                    }
                }
                pc++; break;
            }

            case 'STOP':
                return { pc, cardNum: node.cardNum, nodeType: 'STOP', output: [...output], variables: { ...vars }, done: true, error: null };

            case 'END':
                return { pc, cardNum: node.cardNum, nodeType: 'END', output: [...output], variables: { ...vars }, done: true, error: null };

            case 'UNKNOWN':
                return { pc, cardNum: node.cardNum, nodeType: 'UNKNOWN', output: [...output], variables: { ...vars }, done: true, error: 'Card ' + node.cardNum + ': Unrecognized statement: ' + node.text };

            default:
                pc++; break;
        }
    }

    return { pc: ast.length, cardNum: 0, nodeType: null, output: [...output], variables: { ...vars }, done: true, error: null };
}

function runProgram(cardTexts) {
    const gen = executeProgram(cardTexts);
    let result;
    while (true) {
        const step = gen.next();
        if (step.done) { result = step.value; break; }
    }
    if (!result) return { output: [], error: null };
    return { output: result.output, error: result.error };
}
