// 量子计算遥测测试
// 测试量子计算环境中的遥测数据收集和处理

test "quantum_circuit_execution_telemetry" {
  // 测试量子电路执行遥测
  
  let quantum_execution = [
    ("circuit_name", "grover_search"),
    ("qubit_count", 8),
    ("gate_depth", 25),
    ("execution_time_ms", 1250),
    ("success_probability", 0.87),
    ("measurement_shots", 1024),
    ("fidelity", 0.92),
    ("error_rate", 0.08)
  ]
  
  // 验证量子执行指标
  assert_eq(quantum_execution.length(), 8)
  
  // 验证电路名称
  assert_eq(quantum_execution[0].0, "circuit_name")
  assert_eq(quantum_execution[0].1, "grover_search")
  assert_eq(quantum_execution[0].1.contains("_"), true)
  
  // 验证量子比特数
  assert_eq(quantum_execution[1].0, "qubit_count")
  assert_eq(quantum_execution[1].1, 8)
  assert_eq(quantum_execution[1].1 > 0, true)
  
  // 验证门深度
  assert_eq(quantum_execution[2].0, "gate_depth")
  assert_eq(quantum_execution[2].1, 25)
  assert_eq(quantum_execution[2].1 > 0, true)
  
  // 验证执行时间
  assert_eq(quantum_execution[3].0, "execution_time_ms")
  assert_eq(quantum_execution[3].1, 1250)
  assert_eq(quantum_execution[3].1 > 0, true)
  
  // 验证成功概率
  assert_eq(quantum_execution[4].0, "success_probability")
  assert_eq(quantum_execution[4].1, 0.87)
  assert_eq(quantum_execution[4].1 >= 0.0, true)
  assert_eq(quantum_execution[4].1 <= 1.0, true)
  
  // 验证测量次数
  assert_eq(quantum_execution[5].0, "measurement_shots")
  assert_eq(quantum_execution[5].1, 1024)
  assert_eq(quantum_execution[5].1 > 0, true)
  
  // 验证保真度和错误率
  assert_eq(quantum_execution[6].0, "fidelity")
  assert_eq(quantum_execution[7].0, "error_rate")
  assert_eq(quantum_execution[6].1, 0.92)
  assert_eq(quantum_execution[7].1, 0.08)
  assert_eq(quantum_execution[6].1 >= 0.0, true)
  assert_eq(quantum_execution[6].1 <= 1.0, true)
  assert_eq(quantum_execution[7].1 >= 0.0, true)
  assert_eq(quantum_execution[7].1 <= 1.0, true)
  
  // 验证保真度和错误率关系
  let fidelity_plus_error = quantum_execution[6].1 + quantum_execution[7].1
  assert_eq(fidelity_plus_error <= 1.0, true)
}

test "quantum_error_correction_telemetry" {
  // 测试量子错误纠正遥测
  
  let error_correction = [
    ("code_type", "surface_code"),
    ("physical_qubits", 49),
    ("logical_qubits", 1),
    ("error_correction_cycles", 100),
    ("logical_error_rate", 0.001),
    ("syndrome_extraction_time_ms", 45),
    ("correction_success_rate", 0.985),
    ("overhead_factor", 49.0)
  ]
  
  // 验证错误纠正指标
  assert_eq(error_correction.length(), 8)
  
  // 验证码类型
  assert_eq(error_correction[0].0, "code_type")
  assert_eq(error_correction[0].1, "surface_code")
  assert_eq(error_correction[0].1.contains("_"), true)
  
  // 验证物理和逻辑量子比特
  assert_eq(error_correction[1].0, "physical_qubits")
  assert_eq(error_correction[2].0, "logical_qubits")
  assert_eq(error_correction[1].1, 49)
  assert_eq(error_correction[2].1, 1)
  assert_eq(error_correction[1].1 > error_correction[2].1, true)
  
  // 验证错误纠正周期
  assert_eq(error_correction[3].0, "error_correction_cycles")
  assert_eq(error_correction[3].1, 100)
  assert_eq(error_correction[3].1 > 0, true)
  
  // 验证逻辑错误率
  assert_eq(error_correction[4].0, "logical_error_rate")
  assert_eq(error_correction[4].1, 0.001)
  assert_eq(error_correction[4].1 >= 0.0, true)
  assert_eq(error_correction[4].1 <= 1.0, true)
  
  // 验证综合提取时间
  assert_eq(error_correction[5].0, "syndrome_extraction_time_ms")
  assert_eq(error_correction[5].1, 45)
  assert_eq(error_correction[5].1 > 0, true)
  
  // 验证纠正成功率
  assert_eq(error_correction[6].0, "correction_success_rate")
  assert_eq(error_correction[6].1, 0.985)
  assert_eq(error_correction[6].1 >= 0.0, true)
  assert_eq(error_correction[6].1 <= 1.0, true)
  
  // 验证开销因子
  assert_eq(error_correction[7].0, "overhead_factor")
  assert_eq(error_correction[7].1, 49.0)
  assert_eq(error_correction[7].1 > 1.0, true)
  
  // 验证开销因子计算
  let calculated_overhead = error_correction[1].1 / error_correction[2].1
  assert_eq(calculated_overhead, error_correction[7].1)
}

test "quantum_hardware_calibration_telemetry" {
  // 测试量子硬件校准遥测
  
  let hardware_calibration = [
    ("device_id", "quantum_processor_qpu_01"),
    ("calibration_type", "t1_t2_measurement"),
    ("avg_t1_time_us", 85.5),
    ("avg_t2_time_us", 72.3),
    ("gate_fidelity_single_qubit", 0.998),
    ("gate_fidelity_two_qubit", 0.975),
    ("readout_fidelity", 0.985),
    ("crosstalk_error_rate", 0.002)
  ]
  
  // 验证硬件校准指标
  assert_eq(hardware_calibration.length(), 8)
  
  // 验证设备ID
  assert_eq(hardware_calibration[0].0, "device_id")
  assert_eq(hardware_calibration[0].1, "quantum_processor_qpu_01")
  assert_eq(hardware_calibration[0].1.contains("_"), true)
  
  // 验证校准类型
  assert_eq(hardware_calibration[1].0, "calibration_type")
  assert_eq(hardware_calibration[1].1, "t1_t2_measurement")
  assert_eq(hardware_calibration[1].1.contains("_"), true)
  
  // 验证T1和T2时间
  assert_eq(hardware_calibration[2].0, "avg_t1_time_us")
  assert_eq(hardware_calibration[3].0, "avg_t2_time_us")
  assert_eq(hardware_calibration[2].1, 85.5)
  assert_eq(hardware_calibration[3].1, 72.3)
  assert_eq(hardware_calibration[2].1 > hardware_calibration[3].1, true)
  
  // 验证单量子比特门保真度
  assert_eq(hardware_calibration[4].0, "gate_fidelity_single_qubit")
  assert_eq(hardware_calibration[4].1, 0.998)
  assert_eq(hardware_calibration[4].1 >= 0.0, true)
  assert_eq(hardware_calibration[4].1 <= 1.0, true)
  
  // 验证双量子比特门保真度
  assert_eq(hardware_calibration[5].0, "gate_fidelity_two_qubit")
  assert_eq(hardware_calibration[5].1, 0.975)
  assert_eq(hardware_calibration[5].1 >= 0.0, true)
  assert_eq(hardware_calibration[5].1 <= 1.0, true)
  assert_eq(hardware_calibration[4].1 > hardware_calibration[5].1, true)
  
  // 验证读出保真度
  assert_eq(hardware_calibration[6].0, "readout_fidelity")
  assert_eq(hardware_calibration[6].1, 0.985)
  assert_eq(hardware_calibration[6].1 >= 0.0, true)
  assert_eq(hardware_calibration[6].1 <= 1.0, true)
  
  // 验证串扰错误率
  assert_eq(hardware_calibration[7].0, "crosstalk_error_rate")
  assert_eq(hardware_calibration[7].1, 0.002)
  assert_eq(hardware_calibration[7].1 >= 0.0, true)
  assert_eq(hardware_calibration[7].1 <= 1.0, true)
  
  // 验证T1/T2比例
  let t1_t2_ratio = hardware_calibration[2].1 / hardware_calibration[3].1
  assert_eq(t1_t2_ratio > 1.0, true)
  assert_eq(t1_t2_ratio < 2.0, true)
}

test "quantum_algorithm_performance_telemetry" {
  // 测试量子算法性能遥测
  
  let algorithm_performance = [
    ("algorithm_name", "quantum_fourier_transform"),
    ("problem_size", 16),
    ("classical_time_ms", 5000),
    ("quantum_time_ms", 850),
    ("speedup_factor", 5.88),
    ("accuracy", 0.94),
    ("quantum_volume", 128),
    ("circuit_optimization_applied", true)
  ]
  
  // 验证算法性能指标
  assert_eq(algorithm_performance.length(), 8)
  
  // 验证算法名称
  assert_eq(algorithm_performance[0].0, "algorithm_name")
  assert_eq(algorithm_performance[0].1, "quantum_fourier_transform")
  assert_eq(algorithm_performance[0].1.contains("_"), true)
  
  // 验证问题规模
  assert_eq(algorithm_performance[1].0, "problem_size")
  assert_eq(algorithm_performance[1].1, 16)
  assert_eq(algorithm_performance[1].1 > 0, true)
  
  // 验证经典和量子计算时间
  assert_eq(algorithm_performance[2].0, "classical_time_ms")
  assert_eq(algorithm_performance[3].0, "quantum_time_ms")
  assert_eq(algorithm_performance[2].1, 5000)
  assert_eq(algorithm_performance[3].1, 850)
  assert_eq(algorithm_performance[2].1 > algorithm_performance[3].1, true)
  
  // 验证加速因子
  assert_eq(algorithm_performance[4].0, "speedup_factor")
  assert_eq(algorithm_performance[4].1, 5.88)
  assert_eq(algorithm_performance[4].1 > 1.0, true)
  
  // 验证准确性
  assert_eq(algorithm_performance[5].0, "accuracy")
  assert_eq(algorithm_performance[5].1, 0.94)
  assert_eq(algorithm_performance[5].1 >= 0.0, true)
  assert_eq(algorithm_performance[5].1 <= 1.0, true)
  
  // 验证量子体积
  assert_eq(algorithm_performance[6].0, "quantum_volume")
  assert_eq(algorithm_performance[6].1, 128)
  assert_eq(algorithm_performance[6].1 > 0, true)
  
  // 验证电路优化应用
  assert_eq(algorithm_performance[7].0, "circuit_optimization_applied")
  assert_eq(algorithm_performance[7].1, true)
  
  // 验证加速因子计算
  let calculated_speedup = algorithm_performance[2].1 / algorithm_performance[3].1
  assert_eq(calculated_speedup, algorithm_performance[4].1)
  
  // 验证性能提升
  let performance_improvement = ((algorithm_performance[2].1 - algorithm_performance[3].1) * 100) / algorithm_performance[2].1
  assert_eq(performance_improvement > 80.0, true)
}