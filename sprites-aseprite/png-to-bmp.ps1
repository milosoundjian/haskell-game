Get-ChildItem -Filter "*.png" | ForEach-Object { magick $_.Name ($_.BaseName + '.bmp') }
