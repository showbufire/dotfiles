pushd $vcpath
cmd /c "$vcvars&set" |
foreach {
  if ($_ -match "=") {
    $v = $_.split("="); set-item -force -path "ENV:\$($v[0])"  -value "$($v[1])"
  }
}
popd
write-host "Visual Studio 2010 Command Prompt variables set." -ForegroundColor Magenta