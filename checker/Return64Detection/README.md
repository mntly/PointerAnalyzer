# Heuristics for determining EDX used as return value

First, apply `Modular Analysis` to filter the function which sets EDX before return. Among them, check whether caller uses EDX after calling target function. If caller uses EDX before defining, determine target function returns 64 bit value.

## Modular Analaysis

> If EAX and EDX are valid return value at the end of all return leaf nodes then corresponding function may return 64 bit value.

### Heuristics for checking valid return value at each leaf node

1. EAX/EDX is live at the end of leaf node.
2. EAX/EDX is not used except computing FLAG/Temp registers.
3. FLAG/Temp registers computed using EAX/EDX are not used except computing FLAG/Temp registers.
4. ...

## Caller-Callee Relationship

> For each callsite, transfer CFG to check EDX is used before defining. If EDX is used before defining, corresponding callee function is marked as returning 64 bit value.

### Heuristics for checking EDX uses starting from each callsite

1. If current statement literaly uses EDX, mark EDX is used and stop transferring statments. The word `literaly` means that it does not accept usage on PHI because PHI can not represent the usage of target EDX, and it just merges EDX in different branches.
2. If current statement defines EDX, mark EDX is overwritten and stop transferring statements. PHI on EDX also handled as defining EDX. Since EDX is caller-saved register, function call also handled as defining EDX.
3. If current statement is empty, i.e. until analyzer reaches the end of function, there does not exist neither EDX usage nor definition, mark as UnknownCalle.

## Ground-Truth Evaluation

The Return64 evaluator parses source-level GT with the same recursive parser
as PointerAnalyzer's evaluator and converts it according to the binary ABI
before classifying the expected return width.

For ELF x86-32:

- A normal two-word, eight-byte return is `Return64`.
- A normal return no larger than one word is `Return32`.
- A structure return is `Return32` for this detector because the ABI passes a
  hidden return-buffer pointer instead of returning the structure through
  `EDX:EAX`.
- Unsupported normal return sizes and multiple return entries are invalid GT.

The converted GT used during evaluation is stored as:

```text
SUFFIX_Return64ConvertedGT.json
```

This artifact shows ABI slots and the synthetic hidden return-buffer argument.
