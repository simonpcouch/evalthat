# judges works

    Code
      single
    Output
      Model judge gpt4o.

---

    Code
      multiple
    Output
      Model judges gpt4o and claude.

# inherits names from expressions

    Code
      res
    Output
      Model judges chat_openai(model = "gpt-4o") and chat_claude().

# mixed naming works

    Code
      res
    Output
      Model judges gpt4o and chat_claude().

# fails informatively with non-Chat input

    Code
      judges(not_chat)
    Condition
      Error in `judges()`:
      ! `not_chat` must be an ellmer Chat, not a an empty list

---

    Code
      judges(x = not_chat)
    Condition
      Error in `judges()`:
      ! `x` must be an ellmer Chat, not a an empty list

