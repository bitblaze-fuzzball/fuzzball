(* Logging functionality goes here *)
let default_out = Pervasives.stdout

module FuzzballGeneralCfg = struct
  let level = `Always
  and major_name = "Fuzzball"
  and minor_name = "General"
  and use_hr_time = false
  and out_channel = default_out
end


module StandardTestLogger = struct
  let major_name = "TestLogger"
  and minor_name = "stdout"
  and use_hr_time = false
  and out_channel = Pervasives.stdout
end
