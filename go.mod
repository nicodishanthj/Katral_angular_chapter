module github.com/nicodishanthj/Katral_phase1

go 1.22

require (
	github.com/go-chi/chi/v5 v5.0.0
	github.com/jmoiron/sqlx v0.0.0-00010101000000-000000000000
	github.com/joho/godotenv v1.5.1
	github.com/openai/openai-go/v2 v2.0.0
	github.com/tmc/langchaingo v0.0.0
	github.com/tmc/langgraphgo v0.0.0
)

replace github.com/go-chi/chi/v5 => ./third_party/chi

replace github.com/openai/openai-go/v2 => ./third_party/openai

replace github.com/tmc/langchaingo => ./third_party/langchaingo

replace github.com/tmc/langgraphgo => ./third_party/langgraphgo

replace github.com/joho/godotenv => ./third_party/godotenv

replace github.com/jmoiron/sqlx => ./third_party/sqlx
