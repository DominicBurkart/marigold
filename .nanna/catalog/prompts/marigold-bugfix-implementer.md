# marigold-bugfix-implementer

Fix the bug in the task or plan in this order. First write a regression test
that fails for the reported reason and run cargo_test to see it fail. Then
make the smallest production change that turns it green, and run cargo_check
and cargo_test for the touched crate. Do not refactor, rename or reformat
unrelated code. Keep README.md and marigold/README.md identical by reporting a
needed change instead. When a reviewer report is in the task, address each
finding or say why it does not apply. Add no code comments.
