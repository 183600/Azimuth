// Azimuth Telemetry Analytics Test Suite
// 遥测分析测试套件 - 测试遥测数据的分析和洞察功能

// Test 1: 性能指标分析和优化建议
test "performance metrics analysis and optimization recommendations" {
  // 创建性能分析器
  let performance_analyzer = @azimuth.PerformanceAnalyzer::new(
    @azimuth.AnalysisConfig {
      analysis_types: ["latency", "throughput", "error_rate", "resource_utilization"],
      time_window_hours: 24,
      baseline_period_days: 7,
      anomaly_threshold: 2.0,  // 2个标准差
      recommendation_enabled: true
    }
  )
  
  // 创建性能指标数据
  let base_timestamp = 1640995200000  // 基准时间戳
  let performance_data = []
  
  // 生成24小时的性能数据
  for hour = 0; hour < 24; hour = hour + 1 {
    let timestamp = base_timestamp + hour * 3600000  // 每小时
    
    // 模拟每日性能模式 (工作时间高负载，夜间低负载)
    let base_latency = if hour >= 8 and hour <= 18 {
      150.0 + (10.0 * ((hour - 8) / 10.0).sin())  // 工作时间基线150ms，有波动
    } else {
      50.0 + (5.0 * ((hour - 18) / 6.0).sin())       // 夜间基线50ms，有波动
    }
    
    // 添加一些随机波动和异常
    let latency = base_latency + (if hour == 14 { 500.0 } else { 0.0 })  // 下午2点有延迟尖峰
    
    let throughput = if hour >= 8 and hour <= 18 {
      1000.0 + (200.0 * ((hour - 8) / 10.0).sin())  // 工作时间高吞吐量
    } else {
      200.0 + (50.0 * ((hour - 18) / 6.0).sin())      // 夜间低吞吐量
    }
    
    let error_rate = if hour == 14 {
      0.05  // 下午2点有错误率尖峰
    } else {
      0.01  // 正常错误率
    }
    
    let cpu_utilization = if hour >= 8 and hour <= 18 {
      70.0 + (10.0 * ((hour - 8) / 10.0).sin())  // 工作时间高CPU使用率
    } else {
      30.0 + (5.0 * ((hour - 18) / 6.0).sin())      // 夜间低CPU使用率
    }
    
    let memory_utilization = if hour >= 8 and hour <= 18 {
      80.0 + (5.0 * ((hour - 8) / 10.0).sin())  // 工作时间高内存使用率
    } else {
      40.0 + (3.0 * ((hour - 18) / 6.0).sin())      // 夜间低内存使用率
    }
    
    let data_point = @azimuth.PerformanceDataPoint {
      timestamp,
      service_name: "analytics-service",
      instance_id: "instance-001",
      metrics: [
        ("latency_ms", latency),
        ("throughput_rps", throughput),
        ("error_rate", error_rate),
        ("cpu_utilization_percent", cpu_utilization),
        ("memory_utilization_percent", memory_utilization)
      ]
    }
    performance_data = performance_data.push(data_point)
  }
  
  // 添加性能数据到分析器
  for data_point in performance_data {
    @azimuth.PerformanceAnalyzer::add_data_point(performance_analyzer, data_point)
  }
  
  // 执行性能分析
  let analysis_result = @azimuth.PerformanceAnalyzer::analyze(performance_analyzer)
  
  // 验证分析结果
  assert_eq(analysis_result.analysis_period_hours, 24)
  assert_eq(analysis_result.data_points_count, 24)
  assert_eq(analysis_result.analyzed_metrics.length(), 5)
  
  // 验证延迟分析
  let latency_analysis = analysis_result.analyzed_metrics.find(fn(m) { m.metric_name == "latency_ms" })
  match latency_analysis {
    Some(analysis) => {
      assert_true(analysis.average_value > 100.0)  // 平均延迟应该大于100ms
      assert_true(analysis.min_value > 40.0)      // 最小延迟应该大于40ms
      assert_eq(analysis.max_value, 650.0)        // 最大延迟应该是650ms (150+500)
      assert_true(analysis.standard_deviation > 50.0)  // 应该有显著的标准差
      
      // 验证异常检测
      assert_eq(analysis.anomalies.length(), 1)   // 应该检测到下午2点的延迟尖峰
      let latency_anomaly = analysis.anomalies[0]
      assert_eq(latency_anomaly.timestamp, base_timestamp + 14 * 3600000)  // 下午2点
      assert_eq(latency_anomaly.metric_value, 650.0)
      assert_true(latency_anomaly.anomaly_score > 3.0)  // 高异常分数
      assert_eq(latency_anomaly.anomaly_type, "spike")
    }
    None => assert_true(false)
  }
  
  // 验证吞吐量分析
  let throughput_analysis = analysis_result.analyzed_metrics.find(fn(m) { m.metric_name == "throughput_rps" })
  match throughput_analysis {
    Some(analysis) => {
      assert_true(analysis.average_value > 500.0)  // 平均吞吐量应该大于500rps
      assert_eq(analysis.min_value, 150.0)         // 最小吞吐量
      assert_true(analysis.max_value > 1200.0)     // 最大吞吐量
      assert_eq(analysis.anomalies.length(), 0)    // 吞吐量没有异常
    }
    None => assert_true(false)
  }
  
  // 验证错误率分析
  let error_rate_analysis = analysis_result.analyzed_metrics.find(fn(m) { m.metric_name == "error_rate" })
  match error_rate_analysis {
    Some(analysis) => {
      assert_eq(analysis.average_value, 0.01 + (0.04 / 24.0))  // 平均错误率
      assert_eq(analysis.min_value, 0.01)                      // 最小错误率
      assert_eq(analysis.max_value, 0.05)                      // 最大错误率 (下午2点)
      
      // 验证异常检测
      assert_eq(analysis.anomalies.length(), 1)   // 应该检测到下午2点的错误率尖峰
      let error_anomaly = analysis.anomalies[0]
      assert_eq(error_anomaly.timestamp, base_timestamp + 14 * 3600000)  // 下午2点
      assert_eq(error_anomaly.metric_value, 0.05)
      assert_eq(error_anomaly.anomaly_type, "spike")
    }
    None => assert_true(false)
  }
  
  // 验证优化建议
  assert_eq(analysis_result.recommendations.length(), 3)
  
  // 验证延迟优化建议
  let latency_recommendation = analysis_result.recommendations.find(fn(r) { r.metric == "latency_ms" })
  match latency_recommendation {
    Some(recommendation) => {
      assert_eq(recommendation.priority, "high")
      assert_eq(recommendation.recommendation_type, "performance_optimization")
      assert_true(recommendation.description.contains("下午2点"))
      assert_true(recommendation.description.contains("延迟尖峰"))
      assert_eq(recommendation.suggested_actions.length(), 3)
      assert_true(recommendation.suggested_actions.contains("增加缓存"))
      assert_true(recommendation.suggested_actions.contains("优化数据库查询"))
      assert_true(recommendation.suggested_actions.contains("增加资源"))
    }
    None => assert_true(false)
  }
  
  // 验证错误率优化建议
  let error_rate_recommendation = analysis_result.recommendations.find(fn(r) { r.metric == "error_rate" })
  match error_rate_recommendation {
    Some(recommendation) => {
      assert_eq(recommendation.priority, "high")
      assert_eq(recommendation.recommendation_type, "reliability_improvement")
      assert_true(recommendation.description.contains("错误率尖峰"))
      assert_eq(recommendation.suggested_actions.length(), 2)
      assert_true(recommendation.suggested_actions.contains("增加重试机制"))
      assert_true(recommendation.suggested_actions.contains("实施熔断器"))
    }
    None => assert_true(false)
  }
  
  // 验证资源利用率优化建议
  let resource_recommendation = analysis_result.recommendations.find(fn(r) { r.metric == "memory_utilization_percent" })
  match resource_recommendation {
    Some(recommendation) => {
      assert_eq(recommendation.priority, "medium")
      assert_eq(recommendation.recommendation_type, "resource_optimization")
      assert_true(recommendation.description.contains("内存使用率"))
      assert_true(recommendation.suggested_actions.contains("优化内存使用"))
    }
    None => assert_true(false)
  }
}

// Test 2: 用户行为分析和业务洞察
test "user behavior analysis and business insights" {
  // 创建用户行为分析器
  let behavior_analyzer = @azimuth.UserBehaviorAnalyzer::new(
    @azimuth.BehaviorAnalysisConfig {
      analysis_types: ["journey_mapping", "feature_adoption", "retention", "conversion"],
      time_window_days: 30,
      user_segmentation: true,
      funnel_analysis: true,
      cohort_analysis: true
    }
  )
  
  // 创建用户行为数据
  let base_date = 1640995200000  // 基准日期
  let behavior_data = []
  
  // 生成30天的用户行为数据
  for day = 0; day < 30; day = day + 1 {
    let day_timestamp = base_date + day * 86400000  // 每天的时间戳
    
    // 模拟不同用户类型的行为
    let new_user_count = if day < 7 { 100 - day * 10 } else { 30 }  // 前7天有新用户，之后稳定
    let active_user_count = 500 + (day * 5)  // 活跃用户数逐渐增长
    let returning_user_count = active_user_count - new_user_count
    
    // 模拟功能使用情况
    let feature_a_adoption = if day < 10 { day * 10.0 } else { 100.0 }  // 功能A逐渐采用
    let feature_b_adoption = if day < 15 { day * 5.0 } else { 75.0 }    // 功能B较慢采用
    let feature_c_adoption = if day < 20 { day * 3.0 } else { 60.0 }    // 功能C更慢采用
    
    // 模拟转化漏斗
    let visitors = 1000 + (day * 20)
    let signups = visitors * 0.1  // 10%注册率
    let activations = signups * 0.8  // 80%激活率
    let purchases = activations * 0.2  // 20%购买率
    
    // 模拟留存率
    let day_1_retention = 0.8   // 第1天留存80%
    let day_7_retention = 0.5   // 第7天留存50%
    let day_30_retention = 0.2  // 第30天留存20%
    
    let data_point = @azimuth.UserBehaviorDataPoint {
      timestamp: day_timestamp,
      metrics: [
        ("new_users", new_user_count.to_double()),
        ("active_users", active_user_count.to_double()),
        ("returning_users", returning_user_count.to_double()),
        ("feature_a_adoption_percent", feature_a_adoption),
        ("feature_b_adoption_percent", feature_b_adoption),
        ("feature_c_adoption_percent", feature_c_adoption),
        ("visitors", visitors.to_double()),
        ("signups", signups),
        ("activations", activations),
        ("purchases", purchases),
        ("day_1_retention_rate", day_1_retention),
        ("day_7_retention_rate", day_7_retention),
        ("day_30_retention_rate", day_30_retention)
      ]
    }
    behavior_data = behavior_data.push(data_point)
  }
  
  // 添加用户行为数据到分析器
  for data_point in behavior_data {
    @azimuth.UserBehaviorAnalyzer::add_data_point(behavior_analyzer, data_point)
  }
  
  // 执行用户行为分析
  let behavior_analysis = @azimuth.UserBehaviorAnalyzer::analyze(behavior_analyzer)
  
  // 验证用户增长分析
  assert_eq(behavior_analysis.user_growth_analysis.total_new_users, 565)  // 前7天新用户总和
  assert_eq(behavior_analysis.user_growth_analysis.total_active_users, 635)  // 最后一天的活跃用户数
  assert_true(behavior_analysis.user_growth_analysis.growth_rate > 0.0)  // 应该有正增长
  
  // 验证功能采用分析
  assert_eq(behavior_analysis.feature_adoption_analysis.length(), 3)
  
  let feature_a_adoption = behavior_analysis.feature_adoption_analysis.find(fn(f) { f.feature_name == "feature_a" })
  match feature_a_adoption {
    Some(adoption) => {
      assert_eq(adoption.adoption_rate, 100.0)  // 功能A完全采用
      assert_eq(adoption.adoption_trend, "increasing")  // 采用趋势增加
      assert_true(adoption.time_to_80_percent_adoption <= 10)  // 10天内达到80%采用
    }
    None => assert_true(false)
  }
  
  let feature_b_adoption = behavior_analysis.feature_adoption_analysis.find(fn(f) { f.feature_name == "feature_b" })
  match feature_b_adoption {
    Some(adoption) => {
      assert_eq(adoption.adoption_rate, 75.0)  // 功能B75%采用
      assert_eq(adoption.adoption_trend, "increasing")  // 采用趋势增加
      assert_true(adoption.time_to_80_percent_adoption > 15)  // 超过15天达到80%采用
    }
    None => assert_true(false)
  }
  
  // 验证转化漏斗分析
  assert_eq(behavior_analysis.conversion_funnel_analysis.funnel_stages.length(), 4)
  
  let visitors_stage = behavior_analysis.conversion_funnel_analysis.funnel_stages[0]
  assert_eq(visitors_stage.stage_name, "visitors")
  assert_eq(visitors_stage.user_count, 1580)  // 最后一天的访客数
  
  let signups_stage = behavior_analysis.conversion_funnel_analysis.funnel_stages[1]
  assert_eq(signups_stage.stage_name, "signups")
  assert_eq(signups_stage.user_count, 158)    // 10%注册率
  assert_eq(signups_stage.conversion_rate, 0.1)
  
  let activations_stage = behavior_analysis.conversion_funnel_analysis.funnel_stages[2]
  assert_eq(activations_stage.stage_name, "activations")
  assert_eq(activations_stage.user_count, 126)    // 80%激活率
  assert_eq(activations_stage.conversion_rate, 0.8)
  
  let purchases_stage = behavior_analysis.conversion_funnel_analysis.funnel_stages[3]
  assert_eq(purchases_stage.stage_name, "purchases")
  assert_eq(purchases_stage.user_count, 25)      // 20%购买率
  assert_eq(purchases_stage.conversion_rate, 0.2)
  
  // 验证总体转化率
  assert_eq(behavior_analysis.conversion_funnel_analysis.overall_conversion_rate, 0.016)  // 25/1580 ≈ 1.6%
  
  // 验证留存分析
  assert_eq(behavior_analysis.retention_analysis.day_1_retention, 0.8)
  assert_eq(behavior_analysis.retention_analysis.day_7_retention, 0.5)
  assert_eq(behavior_analysis.retention_analysis.day_30_retention, 0.2)
  
  // 验证用户细分
  assert_eq(behavior_analysis.user_segments.length(), 3)
  
  let power_users = behavior_analysis.user_segments.find(fn(s) { s.segment_name == "power_users" })
  match power_users {
    Some(segment) => {
      assert_eq(segment.segment_size, 127)  // 20%的活跃用户
      assert_eq(segment.defining_characteristics.length(), 2)
      assert_true(segment.defining_characteristics.contains("高活跃度"))
      assert_true(segment.defining_characteristics.contains("功能采用率高"))
    }
    None => assert_true(false)
  }
  
  let casual_users = behavior_analysis.user_segments.find(fn(s) { s.segment_name == "casual_users" })
  match casual_users {
    Some(segment) => {
      assert_eq(segment.segment_size, 381)  // 60%的活跃用户
      assert_true(segment.defining_characteristics.contains("中等活跃度"))
    }
    None => assert_true(false)
  }
  
  let at_risk_users = behavior_analysis.user_segments.find(fn(s) { s.segment_name == "at_risk_users" })
  match at_risk_users {
    Some(segment) => {
      assert_eq(segment.segment_size, 127)  // 20%的活跃用户
      assert_true(segment.defining_characteristics.contains("低活跃度"))
      assert_true(segment.defining_characteristics.contains("有流失风险"))
    }
    None => assert_true(false)
  }
  
  // 验证业务洞察
  assert_eq(behavior_analysis.business_insights.length(), 5)
  
  let growth_insight = behavior_analysis.business_insights.find(fn(i) { i.insight_type == "growth" })
  match growth_insight {
    Some(insight) => {
      assert_eq(insight.priority, "high")
      assert_true(insight.description.contains("用户增长"))
      assert_true(insight.recommendation.contains("加大营销投入"))
    }
    None => assert_true(false)
  }
  
  let adoption_insight = behavior_analysis.business_insights.find(fn(i) { i.insight_type == "feature_adoption" })
  match adoption_insight {
    Some(insight) => {
      assert_eq(insight.priority, "medium")
      assert_true(insight.description.contains("功能B"))
      assert_true(insight.recommendation.contains("改进用户引导"))
    }
    None => assert_true(false)
  }
  
  let retention_insight = behavior_analysis.business_insights.find(fn(i) { i.insight_type == "retention" })
  match retention_insight {
    Some(insight) => {
      assert_eq(insight.priority, "high")
      assert_true(insight.description.contains("30天留存率"))
      assert_true(insight.recommendation.contains("提高用户粘性"))
    }
    None => assert_true(false)
  }
}

// Test 3: 系统健康度评估和预测
test "system health assessment and prediction" {
  // 创建健康度评估器
  let health_assessor = @azimuth.HealthAssessor::new(
    @azimuth.HealthAssessmentConfig {
      assessment_types: ["availability", "performance", "reliability", "capacity"],
      prediction_horizon_days: 7,
      health_score_weights: [
        ("availability", 0.4),
        ("performance", 0.3),
        ("reliability", 0.2),
        ("capacity", 0.1)
      ],
      alert_thresholds: [
        ("health_score", 70.0),
        ("availability", 99.0),
        ("performance", 80.0),
        ("reliability", 95.0),
        ("capacity", 80.0)
      ]
    }
  )
  
  // 创建系统健康度数据
  let base_timestamp = 1640995200000  // 基准时间戳
  let health_data = []
  
  // 生成14天的系统健康度数据
  for day = 0; day < 14; day = day + 1 {
    let day_timestamp = base_timestamp + day * 86400000  // 每天的时间戳
    
    // 模拟可用性数据 (第7天有维护窗口)
    let availability = if day == 7 {
      95.0  // 维护日可用性下降
    } else {
      99.9  // 正常日高可用性
    }
    
    // 模拟性能数据 (第5天有性能问题)
    let performance = if day == 5 {
      65.0  // 第5天性能下降
    } else if day == 6 {
      75.0  // 第6天性能部分恢复
    } else {
      92.0  // 正常日良好性能
    }
    
    // 模拟可靠性数据 (第3天和第10天有故障)
    let reliability = if day == 3 or day == 10 {
      85.0  // 故障日可靠性下降
    } else {
      98.0  // 正常日高可靠性
    }
    
    // 模拟容量数据 (逐渐增长)
    let capacity = 90.0 - (day * 2.0)  // 容量逐渐减少
    
    let data_point = @azimuth.HealthDataPoint {
      timestamp: day_timestamp,
      service_name: "core-service",
      metrics: [
        ("availability_percent", availability),
        ("performance_score", performance),
        ("reliability_percent", reliability),
        ("capacity_utilization_percent", capacity)
      ],
      incidents: if day == 3 or day == 5 or day == 7 or day == 10 {
        [
          @azimuth.Incident {
            id: "incident-" + day.to_string(),
            severity: if day == 5 { "high" } else { "medium" },
            type: if day == 7 { "maintenance" } else { "failure" },
            description: if day == 3 { "数据库连接失败" } 
                      else if day == 5 { "响应时间过长" }
                      else if day == 7 { "计划维护" }
                      else { "内存泄漏" },
            start_time: day_timestamp,
            end_time: day_timestamp + 3600000,  // 1小时
            affected_components: ["database", "cache"]
          }
        ]
      } else {
        []
      }
    }
    health_data = health_data.push(data_point)
  }
  
  // 添加健康度数据到评估器
  for data_point in health_data {
    @azimuth.HealthAssessor::add_data_point(health_assessor, data_point)
  }
  
  // 执行健康度评估
  let health_assessment = @azimuth.HealthAssessor::assess(health_assessor)
  
  // 验证健康度评估结果
  assert_eq(health_assessment.assessment_period_days, 14)
  assert_eq(health_assessment.data_points_count, 14)
  
  // 验证整体健康度评分
  assert_true(health_assessment.overall_health_score >= 0.0)
  assert_true(health_assessment.overall_health_score <= 100.0)
  assert_eq(health_assessment.health_grade, "B")  // 基于评分应该是B级
  
  // 验证各维度健康度
  assert_eq(health_assessment.dimension_scores.length(), 4)
  
  let availability_score = health_assessment.dimension_scores.find(fn(s) { s.dimension == "availability" })
  match availability_score {
    Some(score) => {
      assert_true(score.score >= 95.0)  // 高可用性
      assert_eq(score.grade, "A")
      assert_eq(score.weight, 0.4)
    }
    None => assert_true(false)
  }
  
  let performance_score = health_assessment.dimension_scores.find(fn(s) { s.dimension == "performance" })
  match performance_score {
    Some(score) => {
      assert_true(score.score >= 80.0)  // 中等性能
      assert_eq(score.grade, "B")
      assert_eq(score.weight, 0.3)
    }
    None => assert_true(false)
  }
  
  let reliability_score = health_assessment.dimension_scores.find(fn(s) { s.dimension == "reliability" })
  match reliability_score {
    Some(score) => {
      assert_true(score.score >= 90.0)  // 高可靠性
      assert_eq(score.grade, "A")
      assert_eq(score.weight, 0.2)
    }
    None => assert_true(false)
  }
  
  let capacity_score = health_assessment.dimension_scores.find(fn(s) { s.dimension == "capacity" })
  match capacity_score {
    Some(score) => {
      assert_true(score.score >= 60.0)  // 容量紧张
      assert_eq(score.grade, "C")
      assert_eq(score.weight, 0.1)
    }
    None => assert_true(false)
  }
  
  // 验证事件分析
  assert_eq(health_assessment.incident_analysis.total_incidents, 4)
  assert_eq(health_assessment.incident_analysis.high_severity_incidents, 1)
  assert_eq(health_assessment.incident_analysis.medium_severity_incidents, 3)
  assert_eq(health_assessment.incident_analysis.maintenance_incidents, 1)
  
  // 验证平均故障恢复时间
  assert_eq(health_assessment.incident_analysis.average_recovery_time_minutes, 60)
  
  // 验证健康度趋势
  assert_eq(health_assessment.health_trend.trend_direction, "stable")  // 整体趋势稳定
  assert_true(health_assessment.health_trend.trend_strength >= 0.0)
  assert_true(health_assessment.health_trend.trend_strength <= 1.0)
  
  // 验证健康度预测
  assert_eq(health_assessment.health_prediction.prediction_horizon_days, 7)
  assert_true(health_assessment.health_prediction.predicted_health_score >= 0.0)
  assert_true(health_assessment.health_prediction.predicted_health_score <= 100.0)
  
  // 验证风险预测
  assert_eq(health_assessment.health_prediction.risk_predictions.length(), 2)
  
  let capacity_risk = health_assessment.health_prediction.risk_predictions.find(fn(r) { r.risk_type == "capacity" })
  match capacity_risk {
    Some(risk) => {
      assert_eq(risk.probability, 0.8)  // 高概率
      assert_eq(risk.impact, "high")    // 高影响
      assert_eq(risk.time_to_occurrence_days, 5)  // 5天内可能发生
      assert_true(risk.description.contains("容量"))
      assert_true(risk.mitigation_actions.contains("增加资源"))
    }
    None => assert_true(false)
  }
  
  // 验证健康度建议
  assert_eq(health_assessment.recommendations.length(), 3)
  
  let performance_recommendation = health_assessment.recommendations.find(fn(r) { r.category == "performance" })
  match performance_recommendation {
    Some(recommendation) => {
      assert_eq(recommendation.priority, "high")
      assert_true(recommendation.description.contains("性能优化"))
      assert_true(recommendation.actions.contains("优化慢查询"))
    }
    None => assert_true(false)
  }
  
  let capacity_recommendation = health_assessment.recommendations.find(fn(r) { r.category == "capacity" })
  match capacity_recommendation {
    Some(recommendation) => {
      assert_eq(recommendation.priority, "high")
      assert_true(recommendation.description.contains("容量扩展"))
      assert_true(recommendation.actions.contains("增加实例"))
    }
    None => assert_true(false)
  }
}

// Test 4: 成本效益分析和资源优化
test "cost benefit analysis and resource optimization" {
  // 创建成本效益分析器
  let cost_analyzer = @azimuth.CostBenefitAnalyzer::new(
    @azimuth.CostAnalysisConfig {
      analysis_types: ["resource_utilization", "cost_efficiency", "roi"],
      time_period_days: 30,
      currency: "USD",
      cost_categories: ["compute", "storage", "network", "licensing"],
      optimization_targets: ["cost_reduction", "performance_improvement"]
    }
  )
  
  // 创建成本和资源数据
  let base_timestamp = 1640995200000  // 基准时间戳
  let cost_data = []
  
  // 生成30天的成本和资源数据
  for day = 0; day < 30; day = day + 1 {
    let day_timestamp = base_timestamp + day * 86400000  // 每天的时间戳
    
    // 模拟资源使用情况
    let cpu_cores = 50.0
    let memory_gb = 200.0
    let storage_gb = 5000.0
    let network_gb = 1000.0
    
    // 模拟资源利用率
    let cpu_utilization = 65.0 + (10.0 * ((day / 30.0) * 3.14159).sin())  // CPU利用率波动
    let memory_utilization = 75.0 + (5.0 * ((day / 30.0) * 3.14159).sin())  // 内存利用率波动
    let storage_utilization = 60.0 + (day * 0.5)  // 存储利用率逐渐增长
    let network_utilization = 40.0 + (15.0 * ((day / 30.0) * 3.14159).sin())  // 网络利用率波动
    
    // 模拟成本 (每天)
    let compute_cost = cpu_cores * 10.0 * 24.0  // $10/核心/天
    let memory_cost = memory_gb * 0.5 * 24.0    // $0.5/GB/天
    let storage_cost = storage_gb * 0.02        // $0.02/GB/天
    let network_cost = network_gb * 0.05        // $0.05/GB/天
    let licensing_cost = 100.0                  // $100/天许可证费用
    
    let total_cost = compute_cost + memory_cost + storage_cost + network_cost + licensing_cost
    
    // 模拟业务价值
    let requests_served = 1000000.0 + (day * 10000.0)  // 请求量逐渐增长
    let revenue_per_request = 0.001  // 每个请求$0.001收入
    let daily_revenue = requests_served * revenue_per_request
    
    let data_point = @azimuth.CostDataPoint {
      timestamp: day_timestamp,
      resources: [
        ("cpu_cores", cpu_cores),
        ("memory_gb", memory_gb),
        ("storage_gb", storage_gb),
        ("network_gb", network_gb)
      ],
      utilization: [
        ("cpu_utilization_percent", cpu_utilization),
        ("memory_utilization_percent", memory_utilization),
        ("storage_utilization_percent", storage_utilization),
        ("network_utilization_percent", network_utilization)
      ],
      costs: [
        ("compute_cost", compute_cost),
        ("memory_cost", memory_cost),
        ("storage_cost", storage_cost),
        ("network_cost", network_cost),
        ("licensing_cost", licensing_cost),
        ("total_cost", total_cost)
      ],
      business_metrics: [
        ("requests_served", requests_served),
        ("daily_revenue", daily_revenue)
      ]
    }
    cost_data = cost_data.push(data_point)
  }
  
  // 添加成本数据到分析器
  for data_point in cost_data {
    @azimuth.CostBenefitAnalyzer::add_data_point(cost_analyzer, data_point)
  }
  
  // 执行成本效益分析
  let cost_analysis = @azimuth.CostBenefitAnalyzer::analyze(cost_analyzer)
  
  // 验证成本分析结果
  assert_eq(cost_analysis.analysis_period_days, 30)
  assert_eq(cost_analysis.data_points_count, 30)
  
  // 验证总成本
  assert_eq(cost_analysis.total_cost_analysis.total_cost, cost_data.reduce(fn(acc, dp) { 
    acc + dp.costs.find(fn(c) { c.0 == "total_cost" }).get_or((0, 0.0)).1 
  }, 0.0))
  
  // 验证成本分类
  assert_eq(cost_analysis.cost_breakdown.length(), 5)
  
  let compute_cost_breakdown = cost_analysis.cost_breakdown.find(fn(c) { c.category == "compute_cost" })
  match compute_cost_breakdown {
    Some(breakdown) => {
      assert_true(breakdown.total_amount > 0.0)
      assert_true(breakdown.percentage_of_total > 0.4)  // 计算成本应该占总成本的40%以上
    }
    None => assert_true(false)
  }
  
  // 验证资源利用率分析
  assert_eq(cost_analysis.resource_utilization_analysis.length(), 4)
  
  let cpu_util_analysis = cost_analysis.resource_utilization_analysis.find(fn(r) { r.resource_type == "cpu_utilization_percent" })
  match cpu_util_analysis {
    Some(analysis) => {
      assert_true(analysis.average_utilization >= 60.0)  // 平均CPU利用率
      assert_true(analysis.average_utilization <= 70.0)
      assert_eq(analysis.utilization_efficiency, "good")  // 利用率效率良好
    }
    None => assert_true(false)
  }
  
  let storage_util_analysis = cost_analysis.resource_utilization_analysis.find(fn(r) { r.resource_type == "storage_utilization_percent" })
  match storage_util_analysis {
    Some(analysis) => {
      assert_true(analysis.average_utilization >= 60.0)  // 平均存储利用率
      assert_true(analysis.average_utilization <= 75.0)
      assert_eq(analysis.utilization_trend, "increasing")  // 存储利用率趋势增长
    }
    None => assert_true(false)
  }
  
  // 验证成本效率分析
  assert_true(cost_analysis.cost_efficiency_analysis.cost_per_request > 0.0)
  assert_true(cost_analysis.cost_efficiency_analysis.cost_per_request < 0.01)  // 每个请求成本应小于$0.01
  
  assert_true(cost_analysis.cost_efficiency_analysis.cost_per_revenue_ratio > 0.0)
  assert_true(cost_analysis.cost_efficiency_analysis.cost_per_revenue_ratio < 1.0)  // 成本应小于收入
  
  // 验证ROI分析
  assert_true(cost_analysis.roi_analysis.total_revenue > 0.0)
  assert_true(cost_analysis.roi_analysis.total_cost > 0.0)
  assert_true(cost_analysis.roi_analysis.roi_percentage > 0.0)  // ROI应该为正
  
  // 验证优化机会
  assert_eq(cost_analysis.optimization_opportunities.length(), 3)
  
  let cpu_optimization = cost_analysis.optimization_opportunities.find(fn(o) { o.resource_type == "cpu" })
  match cpu_optimization {
    Some(opportunity) => {
      assert_eq(opportunity.optimization_type, "right_sizing")
      assert_eq(opportunity.priority, "medium")
      assert_true(opportunity.potential_savings > 0.0)
      assert_true(opportunity.description.contains("CPU"))
      assert_true(opportunity.recommended_actions.contains("调整实例大小"))
    }
    None => assert_true(false)
  }
  
  let storage_optimization = cost_analysis.optimization_opportunities.find(fn(o) { o.resource_type == "storage" })
  match storage_optimization {
    Some(opportunity) => {
      assert_eq(opportunity.optimization_type, "lifecycle_management")
      assert_eq(opportunity.priority, "high")
      assert_true(opportunity.potential_savings > 0.0)
      assert_true(opportunity.description.contains("存储"))
      assert_true(opportunity.recommended_actions.contains("实施生命周期策略"))
    }
    None => assert_true(false)
  }
  
  // 验证成本预测
  assert_eq(cost_analysis.cost_prediction.prediction_period_days, 30)
  assert_true(cost_analysis.cost_prediction.predicted_total_cost > 0.0)
  assert_true(cost_analysis.cost_prediction.cost_growth_rate > 0.0)  // 预测成本增长
  
  // 验证成本优化建议
  assert_eq(cost_analysis.optimization_recommendations.length(), 4)
  
  let right_sizing_recommendation = cost_analysis.optimization_recommendations.find(fn(r) { r.recommendation_type == "right_sizing" })
  match right_sizing_recommendation {
    Some(recommendation) => {
      assert_eq(recommendation.priority, "high")
      assert_true(recommendation.description.contains("资源调整"))
      assert_true(recommendation.estimated_savings > 0.0)
      assert_true(recommendation.implementation_effort == "medium")
    }
    None => assert_true(false)
  }
  
  let scheduling_recommendation = cost_analysis.optimization_recommendations.find(fn(r) { r.recommendation_type == "scheduling" })
  match scheduling_recommendation {
    Some(recommendation) => {
      assert_eq(recommendation.priority, "medium")
      assert_true(recommendation.description.contains("调度优化"))
      assert_true(recommendation.estimated_savings > 0.0)
      assert_true(recommendation.implementation_effort == "low")
    }
    None => assert_true(false)
  }
}

// Test 5: 自定义查询和报告生成
test "custom queries and report generation" {
  // 创建查询和报告管理器
  let query_manager = @azimuth.QueryReportManager::new(
    @azimuth.QueryReportConfig {
      supported_query_types: ["time_series", "aggregation", "comparison", "correlation"],
      supported_formats: ["json", "csv", "pdf", "html"],
      max_query_results: 10000,
      cache_enabled: true,
      cache_ttl_minutes: 30
    }
  )
  
  // 创建测试数据
  let base_timestamp = 1640995200000  // 基准时间戳
  let test_data = []
  
  // 生成7天的测试数据
  for day = 0; day < 7; day = day + 1 {
    let day_timestamp = base_timestamp + day * 86400000  // 每天的时间戳
    
    for hour = 0; hour < 24; hour = hour + 1 {
      let timestamp = day_timestamp + hour * 3600000  // 每小时
      
      let data_point = @azimuth.QueryableDataPoint {
        timestamp,
        service_name: if hour % 2 == 0 { "service-a" } else { "service-b" },
        environment: if day < 3 { "production" } else { "staging" },
        metrics: [
          ("response_time_ms", (50.0 + (hour * 2.0)).to_double()),
          ("throughput_rps", (100.0 + (day * 10.0)).to_double()),
          ("error_rate", (0.01 + (hour % 5) * 0.002).to_double()),
          ("cpu_utilization", (30.0 + (hour * 2.5)).to_double()),
          ("memory_utilization", (40.0 + (hour * 1.5)).to_double())
        ],
        dimensions: [
          ("region", if hour < 12 { "us-east-1" } else { "us-west-2" }),
          ("instance_type", if hour % 3 == 0 { "large" } else { "medium" })
        ]
      }
      test_data = test_data.push(data_point)
    }
  }
  
  // 添加测试数据到查询管理器
  for data_point in test_data {
    @azimuth.QueryReportManager::add_data_point(query_manager, data_point)
  }
  
  // 测试时间序列查询
  let time_series_query = @azimuth.Query::new(
    "time_series",
    [
      ("metric", "response_time_ms"),
      ("service", "service-a"),
      ("environment", "production"),
      ("start_time", (base_timestamp).to_string()),
      ("end_time", (base_timestamp + 3 * 86400000).to_string())
    ]
  )
  
  let time_series_result = @azimuth.QueryReportManager::execute_query(query_manager, time_series_query)
  
  // 验证时间序列查询结果
  assert_true(time_series_result.success)
  assert_eq(time_series_result.result_type, "time_series")
  assert_eq(time_series_result.data_points.length(), 36)  // 3天 * 12小时/天 (service-a, production)
  
  for data_point in time_series_result.data_points {
    assert_eq(data_point.service_name, "service-a")
    assert_eq(data_point.environment, "production")
    assert_true(data_point.metrics.contains_key("response_time_ms"))
  }
  
  // 测试聚合查询
  let aggregation_query = @azimuth.Query::new(
    "aggregation",
    [
      ("metric", "throughput_rps"),
      ("aggregation", "avg"),
      ("group_by", "service,environment"),
      ("start_time", (base_timestamp).to_string()),
      ("end_time", (base_timestamp + 7 * 86400000).to_string())
    ]
  )
  
  let aggregation_result = @azimuth.QueryReportManager::execute_query(query_manager, aggregation_query)
  
  // 验证聚合查询结果
  assert_true(aggregation_result.success)
  assert_eq(aggregation_result.result_type, "aggregation")
  assert_eq(aggregation_result.aggregated_data.length(), 4)  // service-a/b * production/staging
  
  let service_a_prod = aggregation_result.aggregated_data.find(fn(d) { 
    d.grouping.get("service") == Some("service-a") and d.grouping.get("environment") == Some("production")
  })
  match service_a_prod {
    Some(data) => {
      assert_true(data.aggregated_value > 0.0)
      assert_eq(data.aggregation_function, "avg")
    }
    None => assert_true(false)
  }
  
  // 测试比较查询
  let comparison_query = @azimuth.Query::new(
    "comparison",
    [
      ("metric", "error_rate"),
      ("compare_by", "environment"),
      ("period1_start", (base_timestamp).to_string()),
      ("period1_end", (base_timestamp + 3 * 86400000).to_string()),
      ("period2_start", (base_timestamp + 3 * 86400000).to_string()),
      ("period2_end", (base_timestamp + 7 * 86400000).to_string())
    ]
  )
  
  let comparison_result = @azimuth.QueryReportManager::execute_query(query_manager, comparison_query)
  
  // 验证比较查询结果
  assert_true(comparison_result.success)
  assert_eq(comparison_result.result_type, "comparison")
  assert_eq(comparison_result.comparison_data.length(), 2)  // production, staging
  
  for comparison in comparison_result.comparison_data {
    assert_true(comparison.period1_value >= 0.0)
    assert_true(comparison.period2_value >= 0.0)
    assert_true(comparison.percent_change != 0.0 or comparison.period1_value == comparison.period2_value)
  }
  
  // 测试相关性查询
  let correlation_query = @azimuth.Query::new(
    "correlation",
    [
      ("metric1", "cpu_utilization"),
      ("metric2", "response_time_ms"),
      ("start_time", (base_timestamp).to_string()),
      ("end_time", (base_timestamp + 7 * 86400000).to_string())
    ]
  )
  
  let correlation_result = @azimuth.QueryReportManager::execute_query(query_manager, correlation_query)
  
  // 验证相关性查询结果
  assert_true(correlation_result.success)
  assert_eq(correlation_result.result_type, "correlation")
  assert_true(correlation_result.correlation_coefficient >= -1.0)
  assert_true(correlation_result.correlation_coefficient <= 1.0)
  assert_true(correlation_result.correlation_strength != "unknown")
  
  // 测试报告生成
  let report_config = @azimuth.ReportConfig {
    title: "系统性能周报",
    description: "过去一周的系统性能分析报告",
    queries: [time_series_query, aggregation_query, comparison_query],
    format: "html",
    include_charts: true,
    include_summary: true
  }
  
  let report_result = @azimuth.QueryReportManager::generate_report(query_manager, report_config)
  
  // 验证报告生成结果
  assert_true(report_result.success)
  assert_eq(report_result.format, "html")
  assert_true(report_result.report_content.length() > 0)
  assert_true(report_result.report_content.contains("系统性能周报"))
  assert_true(report_result.report_content.contains("service-a"))
  
  // 测试JSON格式报告
  let json_report_config = { report_config | format: "json", include_charts: false }
  let json_report_result = @azimuth.QueryReportManager::generate_report(query_manager, json_report_config)
  
  // 验证JSON报告
  assert_true(json_report_result.success)
  assert_eq(json_report_result.format, "json")
  assert_true(json_report_result.report_content.length() > 0)
  assert_true(json_report_result.report_content.contains("\"title\":"))
  assert_true(json_report_result.report_content.contains("\"query_results\":"))
  
  // 测试CSV格式报告
  let csv_report_config = { report_config | format: "csv", include_charts: false, include_summary: false }
  let csv_report_result = @azimuth.QueryReportManager::generate_report(query_manager, csv_report_config)
  
  // 验证CSV报告
  assert_true(csv_report_result.success)
  assert_eq(csv_report_result.format, "csv")
  assert_true(csv_report_result.report_content.length() > 0)
  assert_true(csv_report_result.report_content.contains("timestamp"))
  assert_true(csv_report_result.report_content.contains("response_time_ms"))
  
  // 测试查询缓存
  let cached_query_result = @azimuth.QueryReportManager::execute_query(query_manager, time_series_query)
  assert_true(cached_query_result.success)
  assert_true(cached_query_result.from_cache)  // 应该从缓存获取
  
  // 测试缓存的统计信息
  let cache_stats = @azimuth.QueryReportManager::get_cache_stats(query_manager)
  assert_true(cache_stats.hit_count > 0)
  assert_true(cache_stats.miss_count > 0)
  assert_true(cache_stats.hit_rate > 0.0)
}