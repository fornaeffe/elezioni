param([Parameter(Mandatory=$true)][string]$ProcessIdsJson,[Parameter(Mandatory=$true)][int]$LogicalProcessors)
$ErrorActionPreference='Stop'
Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
public static class RendererCpuLimit {
  [StructLayout(LayoutKind.Sequential)] public struct RateInfo { public uint ControlFlags; public uint CpuRate; }
  [DllImport("kernel32.dll", SetLastError=true)] public static extern IntPtr CreateJobObject(IntPtr attributes,string name);
  [DllImport("kernel32.dll", SetLastError=true)] public static extern bool SetInformationJobObject(IntPtr job,int infoClass,ref RateInfo info,uint length);
  [DllImport("kernel32.dll", SetLastError=true)] public static extern IntPtr OpenProcess(uint access,bool inherit,int processId);
  [DllImport("kernel32.dll", SetLastError=true)] public static extern bool AssignProcessToJobObject(IntPtr job,IntPtr process);
  [DllImport("kernel32.dll")] public static extern bool CloseHandle(IntPtr handle);
}
'@
if($LogicalProcessors -lt 1){throw 'Invalid processor count'}
$taskProcessIds=@(ConvertFrom-Json $ProcessIdsJson)
if(!$taskProcessIds.Count -or $taskProcessIds.Count -gt 32){throw 'Expected private benchmark renderer IDs'}
$taskJob=[RendererCpuLimit]::CreateJobObject([IntPtr]::Zero,$null)
if($taskJob -eq [IntPtr]::Zero){throw 'Cannot create CPU-rate job'}
try {
  $taskRate=[RendererCpuLimit+RateInfo]::new()
  $taskRate.ControlFlags=5 # ENABLE | HARD_CAP; never terminate members on close.
  $taskRate.CpuRate=[uint32][Math]::Max(1,[Math]::Floor(10000/(4*$LogicalProcessors)))
  if(![RendererCpuLimit]::SetInformationJobObject($taskJob,15,[ref]$taskRate,8)){throw ('Cannot set CPU cap: '+[Runtime.InteropServices.Marshal]::GetLastWin32Error())}
  foreach($taskProcessId in $taskProcessIds){
    $taskHandle=[RendererCpuLimit]::OpenProcess(0x0501,$false,[int]$taskProcessId)
    if($taskHandle -eq [IntPtr]::Zero){throw ('Cannot open private renderer '+$taskProcessId)}
    try {if(![RendererCpuLimit]::AssignProcessToJobObject($taskJob,$taskHandle)){throw ('Cannot constrain private renderer: '+[Runtime.InteropServices.Marshal]::GetLastWin32Error())}}
    finally {[void][RendererCpuLimit]::CloseHandle($taskHandle)}
  }
  @{processIds=$taskProcessIds;logicalProcessors=$LogicalProcessors;cpuRate=$taskRate.CpuRate;logicalCpuBudget=$taskRate.CpuRate*$LogicalProcessors/10000;method='Windows job-object hard cap on private benchmark renderers'} | ConvertTo-Json -Compress
  [Console]::Out.Flush()
  [void][Console]::ReadLine() # Parent closes stdin after the measured run.
} finally {[void][RendererCpuLimit]::CloseHandle($taskJob)}
