# Set the current directory to your project folder
Set-Location -Path "C:\Users\james\R_Projects\IPL-2024"

# Remove .json and .txt files in specific directories
Remove-Item -Path "C:\Users\james\R_Projects\IPL-2024\Odds-Scraper\Neds\*.json"
# Remove-Item -Path "C:\Users\james\R_Projects\IPL-2024\Data\BET365_HTML\*.txt"

# Execute Python and R scripts
# & "C:/Python311/python.exe" "c:/Users/james/R_Projects/NBA/Odds-Scraper/get_bet365_html.py"
# & "C:/Python311/python.exe" "c:/Users/james/R_Projects/NBA/Odds-Scraper/get_bet365_player.py"

& "C:/Python312/python.exe" "c:/Users/james/R_Projects/IPL-2024/Odds-Scraper/Neds/get_neds_urls.py"
& "Rscript" "OddsScraper\Neds\get_neds_match_urls.R"
& "C:/Python312/python.exe" "c:/Users/james/R_Projects/IPL-2024/Odds-Scraper/Neds/get_match_json.py"