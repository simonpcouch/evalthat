# str.Chat works

    Code
      str(elmer::chat_claude())
    Message
      Claude claude-3-5-sonnet-latest chat

---

    Code
      str(elmer::chat_claude(api_args = list(temperature = 0.7)))
    Message
      Claude claude-3-5-sonnet-latest chat (temperature: 0.7)

---

    Code
      str(elmer::chat_claude(model = "claude-3-5-sonnet-latest"))
    Message
      Claude claude-3-5-sonnet-latest chat

---

    Code
      str(elmer::chat_claude(model = "claude-3-5-sonnet-latest", api_args = list(
        temperature = 0.7)))
    Message
      Claude claude-3-5-sonnet-latest chat (temperature: 0.7)

---

    Code
      str(elmer::chat_openai())
    Message
      Using model = "gpt-4o".
      OpenAI gpt-4o chat

---

    Code
      str(elmer::chat_openai(model = "gpt-4o-mini"))
    Message
      OpenAI gpt-4o-mini chat

