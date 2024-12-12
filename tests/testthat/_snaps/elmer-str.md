# str.Chat works

    Code
      str(elmer::chat_claude())
    Output
      [1] "Claude claude-3-5-sonnet-latest"

---

    Code
      str(elmer::chat_claude(api_args = list(temperature = 0.7)))
    Output
      [1] "Claude claude-3-5-sonnet-latest (temperature: 0.7)"

---

    Code
      str(elmer::chat_claude(model = "claude-3-5-sonnet-latest"))
    Output
      [1] "Claude claude-3-5-sonnet-latest"

---

    Code
      str(elmer::chat_claude(model = "claude-3-5-sonnet-latest", api_args = list(
        temperature = 0.7)))
    Output
      [1] "Claude claude-3-5-sonnet-latest (temperature: 0.7)"

---

    Code
      str(elmer::chat_openai())
    Message
      Using model = "gpt-4o".
    Output
      [1] "OpenAI gpt-4o"

---

    Code
      str(elmer::chat_openai(model = "gpt-4o-mini"))
    Output
      [1] "OpenAI gpt-4o-mini"

