
function Prompt {
    $b = (Get-GitStatus).Branch
    if ([bool]$b) {
        Write-Host "["(Get-Date -Format "yyyy-MM-ddTHH:mm:ss") (Get-Location)"]("$b" )" -ForegroundColor "Green"
        return "> "
    } else {
        Write-Host "["(Get-Date -Format "yyyy-MM-ddTHH:mm:ss") (Get-Location)"]" -ForegroundColor "Green"
        return "> "
    }
}