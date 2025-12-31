// 遥测系统扩展性测试用例

test "telemetry_horizontal_scaling_capability" {
  // 测试遥测系统水平扩展能力
  
  let scaling_scenarios = [
    {"scenario_id": "scenario_001", "initial_nodes": 3, "target_nodes": 10, "load_increase_factor": 3.0, "expected_performance_degradation": 0.1},
    {"scenario_id": "scenario_002", "initial_nodes": 5, "target_nodes": 15, "load_increase_factor": 2.5, "expected_performance_degradation": 0.15},
    {"scenario_id": "scenario_003", "initial_nodes": 2, "target_nodes": 8, "load_increase_factor": 4.0, "expected_performance_degradation": 0.2},
    {"scenario_id": "scenario_004", "initial_nodes": 8, "target_nodes": 20, "load_increase_factor": 2.0, "expected_performance_degradation": 0.05}
  ]
  
  // 验证扩展场景
  assert_eq(scaling_scenarios.length(), 4)
  assert_eq(scaling_scenarios[0].initial_nodes, 3)
  assert_eq(scaling_scenarios[3].target_nodes, 20)
  
  // 水平扩展参数
  let node_provisioning_time_minutes = 5
  let service_discovery_time_seconds = 30
  let load_balancer_convergence_time_seconds = 60
  
  // 验证水平扩展参数
  assert_eq(node_provisioning_time_minutes, 5)
  assert_eq(service_discovery_time_seconds, 30)
  
  // 水平扩展能力测试
  let scaling_results = []
  let mut i = 0
  
  while i < scaling_scenarios.length() {
    let scenario = scaling_scenarios[i]
    let scenario_id = scenario.scenario_id
    let initial_nodes = scenario.initial_nodes
    let target_nodes = scenario.target_nodes
    let load_increase_factor = scenario.load_increase_factor
    let expected_performance_degradation = scenario.expected_performance_degradation
    
    // 计算扩展因子
    let scaling_factor = target_nodes.to_double() / initial_nodes.to_double()
    let nodes_added = target_nodes - initial_nodes
    
    // 模拟扩展过程时间
    let total_provisioning_time = nodes_added * node_provisioning_time_minutes
    let total_scaling_time_seconds = total_provisioning_time * 60 + service_discovery_time_seconds + load_balancer_convergence_time_seconds
    
    // 计算实际性能变化
    let theoretical_capacity_increase = scaling_factor
    let coordination_overhead = if target_nodes <= 5 {
      0.05
    } else if target_nodes <= 10 {
      0.1
    } else if target_nodes <= 15 {
      0.15
    } else {
      0.2
    }
    
    let network_overhead = if scaling_factor <= 2.0 {
      0.02
    } else if scaling_factor <= 3.0 {
      0.05
    } else {
      0.1
    }
    
    let actual_performance_degradation = coordination_overhead + network_overhead
    let effective_capacity_increase = theoretical_capacity_increase * (1.0 - actual_performance_degradation)
    
    // 评估扩展效果
    let scaling_efficiency = effective_capacity_increase / load_increase_factor
    let scaling_successful = scaling_efficiency >= 0.8
    
    // 计算资源利用率
    let resource_utilization_before = 0.7  // 70%利用率
    let resource_utilization_after = (resource_utilization_before * load_increase_factor) / effective_capacity_increase
    let utilization_improvement = resource_utilization_before - resource_utilization_after
    
    scaling_results.push({
      "scenario_id": scenario_id,
      "initial_nodes": initial_nodes,
      "target_nodes": target_nodes,
      "scaling_factor": scaling_factor,
      "nodes_added": nodes_added,
      "total_scaling_time_seconds": total_scaling_time_seconds,
      "actual_performance_degradation": actual_performance_degradation,
      "effective_capacity_increase": effective_capacity_increase,
      "scaling_efficiency": scaling_efficiency,
      "scaling_successful": scaling_successful,
      "resource_utilization_after": resource_utilization_after,
      "utilization_improvement": utilization_improvement
    })
    
    i = i + 1
  }
  
  // 验证扩展结果
  assert_eq(scaling_results.length(), 4)
  
  // 验证具体场景扩展结果
  assert_eq(scaling_results[0].scenario_id, "scenario_001")
  assert_eq(scaling_results[0].scaling_factor, 10.0 / 3.0)
  assert_eq(scaling_results[0].nodes_added, 7)
  assert_eq(scaling_results[0].scaling_efficiency > 0.8, true)
  assert_eq(scaling_results[0].scaling_successful, true)
  
  assert_eq(scaling_results[2].scenario_id, "scenario_003")
  assert_eq(scaling_results[2].scaling_factor, 4.0)
  assert_eq(scaling_results[2].total_scaling_time_seconds, 390)  // 6*5*60 + 30 + 60
  assert_eq(scaling_results[2].utilization_improvement > 0.0, true)
  
  // 计算整体水平扩展效果
  let mut successful_scalings = 0
  let mut average_scaling_efficiency = 0.0
  let mut total_scaling_time = 0
  let mut average_utilization_improvement = 0.0
  i = 0
  
  while i < scaling_results.length() {
    if scaling_results[i].scaling_successful {
      successful_scalings = successful_scalings + 1
    }
    
    average_scaling_efficiency = average_scaling_efficiency + scaling_results[i].scaling_efficiency
    total_scaling_time = total_scaling_time + scaling_results[i].total_scaling_time_seconds
    average_utilization_improvement = average_utilization_improvement + scaling_results[i].utilization_improvement
    
    i = i + 1
  }
  
  average_scaling_efficiency = average_scaling_efficiency / scaling_results.length().to_double()
  let scaling_success_rate = (successful_scalings * 100) / scaling_results.length()
  let average_scaling_time = total_scaling_time / scaling_results.length()
  average_utilization_improvement = average_utilization_improvement / scaling_results.length().to_double()
  
  // 验证整体水平扩展效果
  assert_eq(successful_scalings, 4)
  assert_eq(scaling_success_rate, 100)
  assert_eq(average_scaling_efficiency > 0.8, true)
  assert_eq(average_utilization_improvement > 0.0, true)
  
  // 评估水平扩展能力
  let horizontal_scaling_excellent = scaling_success_rate == 100 and average_scaling_efficiency >= 0.85 and average_scaling_time <= 600
  let horizontal_scaling_good = scaling_success_rate >= 90 and average_scaling_efficiency >= 0.75 and average_scaling_time <= 900
  let horizontal_scaling_acceptable = scaling_success_rate >= 80 and average_scaling_efficiency >= 0.65 and average_scaling_time <= 1200
  let horizontal_scaling_poor = scaling_success_rate < 80 or average_scaling_efficiency < 0.65 or average_scaling_time > 1200
  
  // 验证水平扩展能力
  assert_eq(horizontal_scaling_excellent, true)
  assert_eq(horizontal_scaling_good, true)
  assert_eq(horizontal_scaling_acceptable, true)
  assert_eq(horizontal_scaling_poor, false)
}

test "telemetry_vertical_scaling_capability" {
  // 测试遥测系统垂直扩展能力
  
  let resource_upgrade_scenarios = [
    {"scenario_id": "cpu_upgrade", "initial_cpu_cores": 4, "upgraded_cpu_cores": 16, "expected_performance_gain": 3.5},
    {"scenario_id": "memory_upgrade", "initial_memory_gb": 16, "upgraded_memory_gb": 64, "expected_performance_gain": 2.8},
    {"scenario_id": "storage_upgrade", "initial_storage_iops": 1000, "upgraded_storage_iops": 5000, "expected_performance_gain": 4.2},
    {"scenario_id": "network_upgrade", "initial_network_bandwidth": 1000, "upgraded_network_bandwidth": 10000, "expected_performance_gain": 8.0}
  ]
  
  // 验证资源升级场景
  assert_eq(resource_upgrade_scenarios.length(), 4)
  assert_eq(resource_upgrade_scenarios[0].scenario_id, "cpu_upgrade")
  assert_eq(resource_upgrade_scenarios[3].upgraded_network_bandwidth, 10000)
  
  // 垂直扩展参数
  let resource_hotplug_capability = true
  let configuration_reload_time_seconds = 30
  let performance_stabilization_time_minutes = 2
  
  // 验证垂直扩展参数
  assert_eq(resource_hotplug_capability, true)
  assert_eq(configuration_reload_time_seconds, 30)
  
  // 垂直扩展能力测试
  let vertical_scaling_results = []
  let mut i = 0
  
  while i < resource_upgrade_scenarios.length() {
    let scenario = resource_upgrade_scenarios[i]
    let scenario_id = scenario.scenario_id
    let expected_performance_gain = scenario.expected_performance_gain
    
    // 计算资源升级因子
    let (upgrade_factor, resource_type) = if scenario_id == "cpu_upgrade" {
      (scenario.upgraded_cpu_cores.to_double() / scenario.initial_cpu_cores.to_double(), "cpu")
    } else if scenario_id == "memory_upgrade" {
      (scenario.upgraded_memory_gb.to_double() / scenario.initial_memory_gb.to_double(), "memory")
    } else if scenario_id == "storage_upgrade" {
      (scenario.upgraded_storage_iops.to_double() / scenario.initial_storage_iops.to_double(), "storage")
    } else if scenario_id == "network_upgrade" {
      (scenario.upgraded_network_bandwidth.to_double() / scenario.initial_network_bandwidth.to_double(), "network")
    } else {
      (1.0, "unknown")
    }
    
    // 计算实际性能增益（考虑资源类型特性）
    let actual_performance_gain = if resource_type == "cpu" {
      upgrade_factor * 0.9  // CPU扩展有轻微递减效应
    } else if resource_type == "memory" {
      upgrade_factor * 0.85  // 内存扩展有递减效应
    } else if resource_type == "storage" {
      upgrade_factor * 0.95  // 存储IOPS扩展效果较好
    } else if resource_type == "network" {
      upgrade_factor * 0.92  // 网络扩展效果较好
    } else {
      upgrade_factor * 0.8
    }
    
    // 计算扩展效率
    let scaling_efficiency = actual_performance_gain / expected_performance_gain
    let scaling_successful = scaling_efficiency >= 0.8
    
    // 计算成本效益
    let resource_cost_factor = upgrade_factor
    let performance_cost_ratio = actual_performance_gain / resource_cost_factor
    
    // 计算扩展时间
    let downtime_seconds = if resource_hotplug_capability {
      configuration_reload_time_seconds
    } else {
      configuration_reload_time_seconds + 300  // 需要重启
    }
    
    let total_impact_time_seconds = downtime_seconds + performance_stabilization_time_minutes * 60
    
    // 评估扩展影响
    let service_impact = if downtime_seconds <= 60 {
      "minimal"
    } else if downtime_seconds <= 300 {
      "moderate"
    } else {
      "significant"
    }
    
    vertical_scaling_results.push({
      "scenario_id": scenario_id,
      "resource_type": resource_type,
      "upgrade_factor": upgrade_factor,
      "expected_performance_gain": expected_performance_gain,
      "actual_performance_gain": actual_performance_gain,
      "scaling_efficiency": scaling_efficiency,
      "scaling_successful": scaling_successful,
      "performance_cost_ratio": performance_cost_ratio,
      "total_impact_time_seconds": total_impact_time_seconds,
      "service_impact": service_impact
    })
    
    i = i + 1
  }
  
  // 验证垂直扩展结果
  assert_eq(vertical_scaling_results.length(), 4)
  
  // 验证具体场景扩展结果
  assert_eq(vertical_scaling_results[0].scenario_id, "cpu_upgrade")
  assert_eq(vertical_scaling_results[0].resource_type, "cpu")
  assert_eq(vertical_scaling_results[0].upgrade_factor, 4.0)
  assert_eq(vertical_scaling_results[0].scaling_efficiency > 0.9, true)
  assert_eq(vertical_scaling_results[0].service_impact, "minimal")
  
  assert_eq(vertical_scaling_results[2].scenario_id, "storage_upgrade")
  assert_eq(vertical_scaling_results[2].upgrade_factor, 5.0)
  assert_eq(vertical_scaling_results[2].scaling_successful, true)
  assert_eq(vertical_scaling_results[2].performance_cost_ratio > 0.9, true)
  
  // 计算整体垂直扩展效果
  let mut successful_scalings = 0
  let mut average_scaling_efficiency = 0.0
  let mut minimal_impact_count = 0
  let mut average_performance_cost_ratio = 0.0
  i = 0
  
  while i < vertical_scaling_results.length() {
    if vertical_scaling_results[i].scaling_successful {
      successful_scalings = successful_scalings + 1
    }
    
    if vertical_scaling_results[i].service_impact == "minimal" {
      minimal_impact_count = minimal_impact_count + 1
    }
    
    average_scaling_efficiency = average_scaling_efficiency + vertical_scaling_results[i].scaling_efficiency
    average_performance_cost_ratio = average_performance_cost_ratio + vertical_scaling_results[i].performance_cost_ratio
    
    i = i + 1
  }
  
  average_scaling_efficiency = average_scaling_efficiency / vertical_scaling_results.length().to_double()
  average_performance_cost_ratio = average_performance_cost_ratio / vertical_scaling_results.length().to_double()
  let scaling_success_rate = (successful_scalings * 100) / vertical_scaling_results.length()
  let minimal_impact_rate = (minimal_impact_count * 100) / vertical_scaling_results.length()
  
  // 验证整体垂直扩展效果
  assert_eq(successful_scalings, 4)
  assert_eq(scaling_success_rate, 100)
  assert_eq(average_scaling_efficiency > 0.85, true)
  assert_eq(minimal_impact_count, 4)
  assert_eq(minimal_impact_rate, 100)
  assert_eq(average_performance_cost_ratio > 0.85, true)
  
  // 评估垂直扩展能力
  let vertical_scaling_excellent = scaling_success_rate == 100 and average_scaling_efficiency >= 0.85 and minimal_impact_rate >= 75
  let vertical_scaling_good = scaling_success_rate >= 90 and average_scaling_efficiency >= 0.75 and minimal_impact_rate >= 50
  let vertical_scaling_acceptable = scaling_success_rate >= 80 and average_scaling_efficiency >= 0.65
  let vertical_scaling_poor = scaling_success_rate < 80 or average_scaling_efficiency < 0.65
  
  // 验证垂直扩展能力
  assert_eq(vertical_scaling_excellent, true)
  assert_eq(vertical_scaling_good, true)
  assert_eq(vertical_scaling_acceptable, true)
  assert_eq(vertical_scaling_poor, false)
}

test "telemetry_auto_scaling_capability" {
  // 测试遥测系统自动扩展能力
  
  let auto_scaling_triggers = [
    {"trigger_id": "cpu_trigger", "metric": "cpu_utilization", "threshold": 80.0, "duration_minutes": 5, "scale_out_cooldown": 10, "scale_in_cooldown": 15},
    {"trigger_id": "memory_trigger", "metric": "memory_utilization", "threshold": 85.0, "duration_minutes": 3, "scale_out_cooldown": 8, "scale_in_cooldown": 12},
    {"trigger_id": "request_rate_trigger", "metric": "requests_per_second", "threshold": 1000.0, "duration_minutes": 2, "scale_out_cooldown": 5, "scale_in_cooldown": 10},
    {"trigger_id": "queue_length_trigger", "metric": "queue_length", "threshold": 100.0, "duration_minutes": 1, "scale_out_cooldown": 3, "scale_in_cooldown": 8}
  ]
  
  // 验证自动扩展触发器
  assert_eq(auto_scaling_triggers.length(), 4)
  assert_eq(auto_scaling_triggers[0].metric, "cpu_utilization")
  assert_eq(auto_scaling_triggers[3].threshold, 100.0)
  
  // 自动扩展参数
  let min_nodes = 2
  let max_nodes = 20
  let scale_out_step = 2
  let scale_in_step = 1
  let metric_evaluation_interval_seconds = 30
  
  // 验证自动扩展参数
  assert_eq(min_nodes, 2)
  assert_eq(max_nodes, 20)
  assert_eq(scale_out_step, 2)
  
  // 自动扩展能力测试
  let auto_scaling_results = []
  let mut i = 0
  
  while i < auto_scaling_triggers.length() {
    let trigger = auto_scaling_triggers[i]
    let trigger_id = trigger.trigger_id
    let metric = trigger.metric
    let threshold = trigger.threshold
    let duration_minutes = trigger.duration_minutes
    let scale_out_cooldown = trigger.scale_out_cooldown
    let scale_in_cooldown = trigger.scale_in_cooldown
    
    // 模拟指标变化模式
    let (metric_pattern, peak_value, baseline_value) = if metric == "cpu_utilization" {
      ("bursty", 95.0, 45.0)
    } else if metric == "memory_utilization" {
      ("gradual_increase", 90.0, 60.0)
    } else if metric == "requests_per_second" {
      ("spiky", 1500.0, 500.0)
    } else if metric == "queue_length" {
      ("erratic", 150.0, 30.0)
    } else {
      ("stable", 50.0, 40.0)
    }
    
    // 计算触发频率
    let trigger_frequency = if metric_pattern == "bursty" {
      3  // 每小时触发3次
    } else if metric_pattern == "gradual_increase" {
      1  // 每小时触发1次
    } else if metric_pattern == "spiky" {
      5  // 每小时触发5次
    } else if metric_pattern == "erratic" {
      4  // 每小时触发4次
    } else {
      0
    }
    
    // 计算响应时间
    let detection_time = duration_minutes * 60  // 持续时间检测
    let decision_time = metric_evaluation_interval_seconds  // 评估间隔
    let provisioning_time = 120  // 2分钟
    let total_response_time = detection_time + decision_time + provisioning_time
    
    // 计算扩展精度
    let scale_out_precision = if threshold >= 80.0 {
      0.9  // 高阈值更精确
    } else if threshold >= 60.0 {
      0.8  // 中等阈值
    } else {
      0.7  // 低阈值
    }
    
    let scale_in_precision = scale_out_precision - 0.1  // 扩入通常不如扩出精确
    
    // 计算稳定性
    let oscillation_risk = if scale_out_cooldown < 5 or scale_in_cooldown < 8 {
      "high"
    } else if scale_out_cooldown < 8 or scale_in_cooldown < 12 {
      "medium"
    } else {
      "low"
    }
    
    // 计算资源利用率优化
    let utilization_optimization = if trigger_frequency >= 3 {
      0.85  // 频繁触发优化更好
    } else if trigger_frequency >= 1 {
      0.75
    } else {
      0.65
    }
    
    // 评估自动扩展效果
    let auto_scaling_effectiveness = scale_out_precision * utilization_optimization
    let scaling_reliable = auto_scaling_effectiveness >= 0.7 and total_response_time <= 600
    
    auto_scaling_results.push({
      "trigger_id": trigger_id,
      "metric": metric,
      "metric_pattern": metric_pattern,
      "trigger_frequency_per_hour": trigger_frequency,
      "total_response_time_seconds": total_response_time,
      "scale_out_precision": scale_out_precision,
      "scale_in_precision": scale_in_precision,
      "oscillation_risk": oscillation_risk,
      "utilization_optimization": utilization_optimization,
      "auto_scaling_effectiveness": auto_scaling_effectiveness,
      "scaling_reliable": scaling_reliable
    })
    
    i = i + 1
  }
  
  // 验证自动扩展结果
  assert_eq(auto_scaling_results.length(), 4)
  
  // 验证具体触发器扩展结果
  assert_eq(auto_scaling_results[0].trigger_id, "cpu_trigger")
  assert_eq(auto_scaling_results[0].metric_pattern, "bursty")
  assert_eq(auto_scaling_results[0].trigger_frequency_per_hour, 3)
  assert_eq(auto_scaling_results[0].scale_out_precision, 0.9)
  assert_eq(auto_scaling_results[0].oscillation_risk, "low")
  assert_eq(auto_scaling_results[0].scaling_reliable, true)
  
  assert_eq(auto_scaling_results[2].trigger_id, "request_rate_trigger")
  assert_eq(auto_scaling_results[2].metric_pattern, "spiky")
  assert_eq(auto_scaling_results[2].trigger_frequency_per_hour, 5)
  assert_eq(auto_scaling_results[2].utilization_optimization, 0.85)
  
  // 计算整体自动扩展效果
  let mut reliable_scalings = 0
  let mut low_oscillation_count = 0
  let mut average_effectiveness = 0.0
  let mut average_response_time = 0
  i = 0
  
  while i < auto_scaling_results.length() {
    if auto_scaling_results[i].scaling_reliable {
      reliable_scalings = reliable_scalings + 1
    }
    
    if auto_scaling_results[i].oscillation_risk == "low" {
      low_oscillation_count = low_oscillation_count + 1
    }
    
    average_effectiveness = average_effectiveness + auto_scaling_results[i].auto_scaling_effectiveness
    average_response_time = average_response_time + auto_scaling_results[i].total_response_time_seconds
    
    i = i + 1
  }
  
  average_effectiveness = average_effectiveness / auto_scaling_results.length().to_double()
  average_response_time = average_response_time / auto_scaling_results.length()
  let reliability_rate = (reliable_scalings * 100) / auto_scaling_results.length()
  let stability_rate = (low_oscillation_count * 100) / auto_scaling_results.length()
  
  // 验证整体自动扩展效果
  assert_eq(reliable_scalings, 4)
  assert_eq(reliability_rate, 100)
  assert_eq(low_oscillation_count, 2)
  assert_eq(stability_rate, 50)
  assert_eq(average_effectiveness > 0.7, true)
  assert_eq(average_response_time < 400, true)
  
  // 评估自动扩展能力
  let auto_scaling_excellent = reliability_rate == 100 and average_effectiveness >= 0.8 and stability_rate >= 75
  let auto_scaling_good = reliability_rate >= 90 and average_effectiveness >= 0.7 and stability_rate >= 50
  let auto_scaling_acceptable = reliability_rate >= 80 and average_effectiveness >= 0.6
  let auto_scaling_poor = reliability_rate < 80 or average_effectiveness < 0.6
  
  // 验证自动扩展能力
  assert_eq(auto_scaling_excellent, false)
  assert_eq(auto_scaling_good, true)
  assert_eq(auto_scaling_acceptable, true)
  assert_eq(auto_scaling_poor, false)
}

test "telemetry_elastic_scaling_capability" {
  // 测试遥测系统弹性扩展能力
  
  let elasticity_scenarios = [
    {"scenario_id": "flash_crowd", "initial_load": 100, "peak_load": 1000, "peak_duration_minutes": 15, "ramp_up_time_minutes": 5},
    {"scenario_id": "gradual_growth", "initial_load": 200, "peak_load": 600, "peak_duration_minutes": 120, "ramp_up_time_minutes": 60},
    {"scenario_id": "burst_traffic", "initial_load": 150, "peak_load": 800, "peak_duration_minutes": 10, "ramp_up_time_minutes": 2},
    {"scenario_id": "sustained_high", "initial_load": 300, "peak_load": 900, "peak_duration_minutes": 240, "ramp_up_time_minutes": 30}
  ]
  
  // 验证弹性场景
  assert_eq(elasticity_scenarios.length(), 4)
  assert_eq(elasticity_scenarios[0].scenario_id, "flash_crowd")
  assert_eq(elasticity_scenarios[3].peak_duration_minutes, 240)
  
  // 弹性扩展参数
  let elasticity_factor = 10.0  // 最大扩展倍数
  let elastic_response_time_target_seconds = 60
  let resource_utilization_target = 0.75
  
  // 验证弹性扩展参数
  assert_eq(elasticity_factor, 10.0)
  assert_eq(elastic_response_time_target_seconds, 60)
  
  // 弹性扩展能力测试
  let elasticity_results = []
  let mut i = 0
  
  while i < elasticity_scenarios.length() {
    let scenario = elasticity_scenarios[i]
    let scenario_id = scenario.scenario_id
    let initial_load = scenario.initial_load
    let peak_load = scenario.peak_load
    let peak_duration_minutes = scenario.peak_duration_minutes
    let ramp_up_time_minutes = scenario.ramp_up_time_minutes
    
    // 计算负载变化率
    let load_increase_factor = peak_load.to_double() / initial_load.to_double()
    let load_change_rate = (peak_load - initial_load).to_double() / ramp_up_time_minutes.to_double()
    
    // 计算所需扩展量
    let required_scaling_factor = if load_increase_factor <= elasticity_factor {
      load_increase_factor
    } else {
      elasticity_factor  // 受最大弹性限制
    }
    
    // 计算实际扩展能力
    let provisioning_delay = if scenario_id == "flash_crowd" or scenario_id == "burst_traffic" {
      2  // 快速响应
    } else {
      5  // 标准响应
    }
    
    let actual_response_time_seconds = ramp_up_time_minutes * 60 + provisioning_delay * 60
    
    // 计算扩展精度
    let scaling_precision = if actual_response_time_seconds <= elastic_response_time_target_seconds {
      0.9
    } else if actual_response_time_seconds <= elastic_response_time_target_seconds * 2 {
      0.7
    } else {
      0.5
    }
    
    // 计算扩展效率
    let scaling_efficiency = required_scaling_factor * scaling_precision
    
    // 计算成本效率
    let peak_resource_cost = required_scaling_factor * peak_duration_minutes.to_double()
    let baseline_resource_cost = initial_load.to_double()
    let cost_efficiency = baseline_resource_cost / peak_resource_cost
    
    // 计算弹性恢复能力
    let ramp_down_time_minutes = ramp_up_time_minutes / 2  // 收缩通常更快
    let recovery_capability = if ramp_down_time_minutes <= 10 {
      "excellent"
    } else if ramp_down_time_minutes <= 30 {
      "good"
    } else {
      "acceptable"
    }
    
    // 评估弹性效果
    let elasticity_score = scaling_efficiency * cost_efficiency
    let elasticity_effective = elasticity_score >= 0.5 and scaling_precision >= 0.7
    
    elasticity_results.push({
      "scenario_id": scenario_id,
      "load_increase_factor": load_increase_factor,
      "required_scaling_factor": required_scaling_factor,
      "actual_response_time_seconds": actual_response_time_seconds,
      "scaling_precision": scaling_precision,
      "scaling_efficiency": scaling_efficiency,
      "cost_efficiency": cost_efficiency,
      "elasticity_score": elasticity_score,
      "recovery_capability": recovery_capability,
      "elasticity_effective": elasticity_effective
    })
    
    i = i + 1
  }
  
  // 验证弹性扩展结果
  assert_eq(elasticity_results.length(), 4)
  
  // 验证具体场景弹性结果
  assert_eq(elasticity_results[0].scenario_id, "flash_crowd")
  assert_eq(elasticity_results[0].load_increase_factor, 10.0)
  assert_eq(elasticity_results[0].required_scaling_factor, 10.0)
  assert_eq(elasticity_results[0].scaling_precision, 0.9)
  assert_eq(elasticity_results[0].recovery_capability, "excellent")
  assert_eq(elasticity_results[0].elasticity_effective, true)
  
  assert_eq(elasticity_results[1].scenario_id, "gradual_growth")
  assert_eq(elasticity_results[1].load_increase_factor, 3.0)
  assert_eq(elasticity_results[1].cost_efficiency > 0.2, true)
  
  // 计算整体弹性扩展效果
  let mut effective_elasticity_count = 0
  let mut excellent_recovery_count = 0
  let mut average_elasticity_score = 0.0
  let mut average_scaling_precision = 0.0
  i = 0
  
  while i < elasticity_results.length() {
    if elasticity_results[i].elasticity_effective {
      effective_elasticity_count = effective_elasticity_count + 1
    }
    
    if elasticity_results[i].recovery_capability == "excellent" {
      excellent_recovery_count = excellent_recovery_count + 1
    }
    
    average_elasticity_score = average_elasticity_score + elasticity_results[i].elasticity_score
    average_scaling_precision = average_scaling_precision + elasticity_results[i].scaling_precision
    
    i = i + 1
  }
  
  average_elasticity_score = average_elasticity_score / elasticity_results.length().to_double()
  average_scaling_precision = average_scaling_precision / elasticity_results.length().to_double()
  let elasticity_effectiveness_rate = (effective_elasticity_count * 100) / elasticity_results.length()
  let excellent_recovery_rate = (excellent_recovery_count * 100) / elasticity_results.length()
  
  // 验证整体弹性扩展效果
  assert_eq(effective_elasticity_count, 3)
  assert_eq(elasticity_effectiveness_rate, 75)
  assert_eq(excellent_recovery_count, 2)
  assert_eq(excellent_recovery_rate, 50)
  assert_eq(average_elasticity_score > 0.3, true)
  assert_eq(average_scaling_precision > 0.7, true)
  
  // 评估弹性扩展能力
  let elasticity_excellent = elasticity_effectiveness_rate >= 80 and average_elasticity_score >= 0.5 and excellent_recovery_rate >= 50
  let elasticity_good = elasticity_effectiveness_rate >= 70 and average_elasticity_score >= 0.4 and excellent_recovery_rate >= 25
  let elasticity_acceptable = elasticity_effectiveness_rate >= 60 and average_elasticity_score >= 0.3
  let elasticity_poor = elasticity_effectiveness_rate < 60 or average_elasticity_score < 0.3
  
  // 验证弹性扩展能力
  assert_eq(elasticity_excellent, false)
  assert_eq(elasticity_good, true)
  assert_eq(elasticity_acceptable, true)
  assert_eq(elasticity_poor, false)
}