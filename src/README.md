Note
* Extract Exit Registers from Register States of all Leaf Nodes in CFG.
* There may exist multiple Leaf Nodes.
* If there exist multiple register type id for unique register, merge them with
    below rule.
    1. Caller-Saved Register: It is possible to have different type.
        Only when the type id of each candidates are same, extract to exit register
    2. Calee-Saved Regsiter and Return Regsiter: It must be same type.
        Add Same Type constraint for all candidates.