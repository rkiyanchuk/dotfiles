---
description: Concise-style prose enforcement — no pleasantries, hedging, filler framing, or follow-up offers
scope: [text]
interruptMode: prose-only
condition:
  - "(?i)^\\s*(sure|certainly|of course|absolutely|great question|good question|you'?re right)\\b"
  - "(?i)\\b(I'?d be happy to|happy to help|let me know if|feel free to|hope (this|that) helps|as an AI)\\b"
  - "(?i)\\b(it'?s (important|worth) (to note|noting)|note that it'?s worth|in summary, |to summarize, )"
  - "(?i)\\b(not a coincidence|as (we|you) (can see|might expect)|interestingly(,| enough))\\b"
---

Concise style violated. Rewrite the response:

- Delete the pleasantry, hedge, filler framing sentence, or follow-up offer.
- Lead with the result. Every sentence carries a fact, decision, or risk.
- One fix per problem. No enumerated alternatives unless asked.
