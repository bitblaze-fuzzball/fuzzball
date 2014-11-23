(* Logging functionality goes here *)
let default_out = Pervasives.stdout

let get_chan major minor () =
  match Logger_config.get_logfile_channel (major,minor) with
  | Logger_config.Fixed oc -> oc
  | Logger_config.Incrementing (channel_desc, channel) -> channel

module FuzzballGeneralCfg = struct
  let level = `Always
  and major_name = "Fuzzball"
  and minor_name = "General"
  and use_hr_time = false
  and out_channel = get_chan "Fuzzball" "General"
end

(* This is the logger that captures fuzzball's restart reasons. *)
module FuzzballBDTCfg = struct
  let level = `Always
  and major_name = "BDT"
  and minor_name = "EXP"
  and use_hr_time = true
  and out_channel = get_chan "BDT" "EXP"
end

module FuzzballTimingCfg = struct
  let level = `Always
  and major_name = "Fuzzball"
  and minor_name = "Timing"
  and use_hr_time = true
  and out_channel = get_chan "Fuzzball" "Timing"
end

module StandardTestLogger = struct
  let major_name = "TestLogger"
  and minor_name = "stdout"
  and use_hr_time = false
  and out_channel = get_chan "TestLogger" "stdout"
end
