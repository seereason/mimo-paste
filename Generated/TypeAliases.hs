type App' = FoundationT' (URL AppURL) Acid () IO
type App = XMLGenT App'
type AppForm = FoundationForm (URL AppURL) Acid () IO
