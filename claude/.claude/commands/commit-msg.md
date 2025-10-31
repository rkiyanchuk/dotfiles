# Commit Message Command

Create a commit message for changes that are about to be committed.

## Instructions

- Describe currently staged changes.
- If no changes are staged, describe currently uncommitted changes.
- Use Conventional Commits format.
- Format commits properly according to Git best practices.
- Use imperative form.
- Keep description concise.

## Output format

```
Recommended commit message:

---------
<message>
---------

Would you like to make the commit?
```

If the user answers with `y` or `yes`, create Git commit with the created
message.
