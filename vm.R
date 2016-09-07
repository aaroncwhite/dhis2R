# Some utility functions to list available VMs and start/stop them in headless mode
listVMs <- function(running_only=F) {
  # list available VMs. running_only=T will only list
  # running vms
  if (running_only == T) {vms <- 'runningvms'} else {vms <- 'vms'}
  return(vBoxManage(paste0('list ',vms)))
}



vBoxManage <- function(commands) {
  # Interact with the base utility
  resp <- system(paste('"c:/Program Files/Oracle/VirtualBox/VBoxManage.exe"', commands))
  return(resp)
}