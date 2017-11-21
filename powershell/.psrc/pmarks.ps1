function marks {
    python $pmarks ls
}

function mark($name) {
    python $pmarks mark $name
}

function unmark($name) {
    python $pmarks unmark $name
}

function jump($name) {
    $dest = python $pmarks get $name
    if ($?) {
        Set-Location -Path $dest -PassThru
    }
}

Set-Alias -Name jp -Value jump

write-host "Command line bookmarks loaded." -ForegroundColor Magenta
