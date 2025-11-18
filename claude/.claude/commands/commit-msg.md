# Commit Message Command

Create a commit message for changes that are about to be committed.

## Instructions

1. Understand what was accomplished.

   - Check staged changes to see what has been marked for commit (use `git diff --cached`)
   - Review the conversation history and understand what was accomplished

    If no changes are staged, describe currently uncommitted changes.

    - Run `git status` to see current changes
    - Run `git diff` to understand the modifications

2. Create commit message

   - Focus on why the changes were made, not just what
   - Use imperative style in commit messages
   - Use Conventional Commits format
   - Format commits according to Git best practices
   - Keep description concise

## Output format

    ```
    Recommended commit message:

    ———
    <message>
    ———

    Would you like to make the commit?
    ```

If the user answers with `y` or `yes`, create Git commit with the created
message.

## Important

- NEVER add co-author information or Claude attribution
- Do not include any "Generated with Claude" messages
- Write commit messages as if the user wrote them
