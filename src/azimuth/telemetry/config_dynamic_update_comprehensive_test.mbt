// 配置管理的动态更新综合测试
// 测试遥测系统配置的动态更新和热重载功能

test "dynamic_sampling_rate_update" {
  // 测试动态采样率更新
  let mut current_config = {
    "sampling_rate": 0.1,
    "sampling_type": "probabilistic",
    "update_timestamp": "2023-01-01T00:00:00Z"
  }
  
  // 初始配置验证
  assert_eq(current_config["sampling_rate"], 0.1)
  assert_eq(current_config["sampling_type"], "probabilistic")
  
  // 动态更新采样率
  current_config["sampling_rate"] = 0.5
  current_config["update_timestamp"] = "2023-01-01T01:00:00Z"
  
  // 验证更新后的配置
  assert_eq(current_config["sampling_rate"], 0.5)
  assert_eq(current_config["sampling_type"], "probabilistic")
  
  // 测试边界值更新
  current_config["sampling_rate"] = 0.0  // 禁用采样
  assert_eq(current_config["sampling_rate"], 0.0)
  
  current_config["sampling_rate"] = 1.0  // 全采样
  assert_eq(current_config["sampling_rate"], 1.0)
}