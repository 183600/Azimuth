// Azimuth Telemetry Data Privacy Protection and Anonymization Tests
// 遥测数据隐私保护和脱敏测试用例，专注于敏感数据识别、脱敏和隐私保护

// Test 1: 敏感数据识别和分类
test "sensitive data identification and classification" {
  // 创建敏感数据识别器
  let identifier = azimuth::privacy::Identifier::new()
  
  // 配置敏感数据规则
  azimuth::privacy::add_pattern(identifier, "email", "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")
  azimuth::privacy::add_pattern(identifier, "phone", "^\\+?[1-9]\\d{1,14}$")
  azimuth::privacy::add_pattern(identifier, "credit_card", "^\\d{4}[-\\s]?\\d{4}[-\\s]?\\d{4}[-\\s]?\\d{4}$")
  azimuth::privacy::add_pattern(identifier, "ssn", "^\\d{3}-\\d{2}-\\d{4}$")
  azimuth::privacy::add_pattern(identifier, "ip_address", "^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$")
  
  // 添加敏感关键词
  azimuth::privacy::add_keyword(identifier, "password")
  azimuth::privacy::add_keyword(identifier, "secret")
  azimuth::privacy::add_keyword(identifier, "token")
  azimuth::privacy::add_keyword(identifier, "api_key")
  
  // 创建包含敏感信息的测试数据
  let test_data = [
    ("user_email", "john.doe@example.com"),
    ("user_phone", "+1234567890"),
    ("credit_card", "4111 1111 1111 1111"),
    ("ssn", "123-45-6789"),
    ("ip_address", "192.168.1.1"),
    ("password", "supersecret123"),
    ("api_token", "sk_live_1234567890abcdef"),
    ("normal_data", "this is not sensitive")
  ]
  
  // 识别敏感数据
  let sensitive_fields = []
  for (field_name, field_value) in test_data {
    let sensitivity = azimuth::privacy::identify_sensitivity(identifier, field_name, field_value)
    if azimuth::privacy::is_sensitive(sensitivity) {
      sensitive_fields = sensitive_fields.push((field_name, azimuth::privacy::get_sensitivity_type(sensitivity)))
    }
  }
  
  // 验证敏感数据识别结果
  assert_eq(sensitive_fields.length(), 7) // 应该识别出7个敏感字段
  
  // 检查特定类型的敏感数据
  let email_fields = sensitive_fields.filter(fn((_, t)) { t == "email" })
  let phone_fields = sensitive_fields.filter(fn((_, t)) { t == "phone" })
  let credit_card_fields = sensitive_fields.filter(fn((_, t)) { t == "credit_card" })
  let ssn_fields = sensitive_fields.filter(fn((_, t)) { t == "ssn" })
  let ip_fields = sensitive_fields.filter(fn((_, t)) { t == "ip_address" })
  let keyword_fields = sensitive_fields.filter(fn((_, t)) { t == "keyword" })
  
  assert_eq(email_fields.length(), 1)
  assert_eq(phone_fields.length(), 1)
  assert_eq(credit_card_fields.length(), 1)
  assert_eq(ssn_fields.length(), 1)
  assert_eq(ip_fields.length(), 1)
  assert_eq(keyword_fields.length(), 2) // password和api_token
}

// Test 2: 遮蔽脱敏算法
test "masking anonymization algorithms" {
  // 创建脱敏处理器
  let anonymizer = azimuth::privacy::Anonymizer::new()
  
  // 配置脱敏策略
  azimuth::privacy::set_masking_strategy(anonymizer, "email", "partial", 2, 2) // 保留前2和后2个字符
  azimuth::privacy::set_masking_strategy(anonymizer, "phone", "full") // 完全遮蔽
  azimuth::privacy::set_masking_strategy(anonymizer, "credit_card", "partial", 4, 4) // 保留前4和后4个字符
  azimuth::privacy::set_masking_strategy(anonymizer, "name", "hash") // 哈希处理
  azimuth::privacy::set_masking_strategy(anonymizer, "ip_address", "generalize", 2) // 泛化到子网级别
  
  // 测试数据
  let test_data = [
    ("email", "john.doe@example.com"),
    ("phone", "+1234567890"),
    ("credit_card", "4111111111111111"),
    ("name", "John Doe"),
    ("ip_address", "192.168.1.100")
  ]
  
  // 应用脱敏
  let anonymized_data = []
  for (field_type, value) in test_data {
    let anonymized_value = azimuth::privacy::anonymize(anonymizer, field_type, value)
    anonymized_data = anonymized_data.push((field_type, anonymized_value))
  }
  
  // 验证脱敏结果
  let email_result = anonymized_data.find(fn((t, _)) { t == "email" }).unwrap().1
  let phone_result = anonymized_data.find(fn((t, _)) { t == "phone" }).unwrap().1
  let credit_card_result = anonymized_data.find(fn((t, _)) { t == "credit_card" }).unwrap().1
  let name_result = anonymized_data.find(fn((t, _)) { t == "name" }).unwrap().1
  let ip_result = anonymized_data.find(fn((t, _)) { t == "ip_address" }).unwrap().1
  
  // 验证脱敏效果
  assert_eq(email_result, "jo**********om") // 保留前2和后2个字符
  assert_eq(phone_result, "**********") // 完全遮蔽
  assert_eq(credit_card_result, "4111********1111") // 保留前4和后4个字符
  assert_not_eq(name_result, "John Doe") // 哈希处理，应该不同
  assert_eq(ip_result, "192.168.0.0") // 泛化到子网级别
  
  // 验证脱敏一致性
  let name_anonymized_again = azimuth::privacy::anonymize(anonymizer, "name", "John Doe")
  assert_eq(name_result, name_anonymized_again) // 相同输入应该产生相同的脱敏结果
}

// Test 3: 令牌化脱敏
test "tokenization anonymization" {
  // 创建令牌化处理器
  let tokenizer = azimuth::privacy::Tokenizer::new()
  
  // 配置令牌化
  azimuth::privacy::set_token_algorithm(tokenizer, "AES-256-GCM")
  azimuth::privacy::set_token_key_rotation_interval(tokenizer, 86400) // 24小时轮换密钥
  
  // 创建令牌化映射
  let token_map = azimuth::privacy::TokenMap::new()
  
  // 测试数据
  let sensitive_values = [
    "john.doe@example.com",
    "1234567890",
    "4111111111111111",
    "John Doe",
    "123 Main St, Anytown, USA"
  ]
  
  // 令牌化数据
  let tokens = []
  for value in sensitive_values {
    let token = azimuth::privacy::tokenize(tokenizer, token_map, value)
    tokens = tokens.push((value, token))
  }
  
  // 验证令牌化结果
  for (original, token) in tokens {
    assert_not_eq(original, token) // 令牌应该与原始值不同
    assert_eq(token.length(), 64) // 令牌应该有固定长度
    assert_true(token.matches("^[a-f0-9]+$")) // 令牌应该是十六进制字符串
  }
  
  // 验证令牌唯一性
  let unique_tokens = tokens.map(fn((_, t)) { t }).unique()
  assert_eq(unique_tokens.length(), tokens.length()) // 所有令牌应该是唯一的
  
  // 验证令牌一致性
  let test_value = "john.doe@example.com"
  let token1 = azimuth::privacy::tokenize(tokenizer, token_map, test_value)
  let token2 = azimuth::privacy::tokenize(tokenizer, token_map, test_value)
  assert_eq(token1, token2) // 相同值应该产生相同的令牌
  
  // 验证令牌反令牌化
  let detokenized_value = azimuth::privacy::detokenize(tokenizer, token_map, token1)
  assert_eq(detokenized_value, test_value)
}

// Test 4: 数据泛化脱敏
test "data generalization anonymization" {
  // 创建泛化处理器
  let generalizer = azimuth::privacy::Generalizer::new()
  
  // 配置泛化规则
  azimuth::privacy::add_generalization_rule(generalizer, "age", "range", 10) // 年龄按10年范围泛化
  azimuth::privacy::add_generalization_rule(generalizer, "salary", "range", 10000) // 薪资按10000范围泛化
  azimuth::privacy::add_generalization_rule(generalizer, "timestamp", "hour") // 时间戳按小时泛化
  azimuth::privacy::add_generalization_rule(generalizer, "location", "city") // 位置泛化到城市级别
  
  // 测试数据
  let test_data = [
    ("age", "27"),
    ("age", "34"),
    ("age", "42"),
    ("salary", "75000"),
    ("salary", "82000"),
    ("salary", "95000"),
    ("timestamp", "1640995200000"), // 2022-01-01 00:00:00
    ("timestamp", "1640995300000"), // 2022-01-01 00:01:40
    ("location", "123 Main St, Springfield, IL, USA"),
    ("location", "456 Oak Ave, Shelbyville, IL, USA")
  ]
  
  // 应用泛化
  let generalized_data = []
  for (field_type, value) in test_data {
    let generalized_value = azimuth::privacy::generalize(generalizer, field_type, value)
    generalized_data = generalized_data.push((field_type, value, generalized_value))
  }
  
  // 验证泛化结果
  let age_27 = generalized_data.find(fn((t, v, _)) { t == "age" && v == "27" }).unwrap().2
  let age_34 = generalized_data.find(fn((t, v, _)) { t == "age" && v == "34" }).unwrap().2
  let age_42 = generalized_data.find(fn((t, v, _)) { t == "age" && v == "42" }).unwrap().2
  
  assert_eq(age_27, "20-30") // 27岁泛化为20-30范围
  assert_eq(age_34, "30-40") // 34岁泛化为30-40范围
  assert_eq(age_42, "40-50") // 42岁泛化为40-50范围
  
  let salary_75000 = generalized_data.find(fn((t, v, _)) { t == "salary" && v == "75000" }).unwrap().2
  let salary_82000 = generalized_data.find(fn((t, v, _)) { t == "salary" && v == "82000" }).unwrap().2
  
  assert_eq(salary_75000, "70000-80000") // 75000泛化为70000-80000范围
  assert_eq(salary_82000, "80000-90000") // 82000泛化为80000-90000范围
  
  let timestamp1 = generalized_data.find(fn((t, v, _)) { t == "timestamp" && v == "1640995200000" }).unwrap().2
  let timestamp2 = generalized_data.find(fn((t, v, _)) { t == "timestamp" && v == "1640995300000" }).unwrap().2
  
  assert_eq(timestamp1, "2022-01-01 00:00:00") // 第一个时间戳
  assert_eq(timestamp2, "2022-01-01 00:00:00") // 两个时间戳应该泛化到同一小时
  
  let location1 = generalized_data.find(fn((t, v, _)) { t == "location" && v.contains("Springfield") }).unwrap().2
  let location2 = generalized_data.find(fn((t, v, _)) { t == "location" && v.contains("Shelbyville") }).unwrap().2
  
  assert_eq(location1, "Springfield, IL, USA") // 位置泛化到城市级别
  assert_eq(location2, "Shelbyville, IL, USA")
}

// Test 5: 噪声注入脱敏
test "noise injection anonymization" {
  // 创建噪声注入处理器
  let noise_injector = azimuth::privacy::NoiseInjector::new()
  
  // 配置噪声参数
  azimuth::privacy::set_noise_distribution(noise_injector, "laplace") // 拉普拉斯分布
  azimuth::privacy::set_noise_scale(noise_injector, 1.0) // 噪声尺度参数
  azimuth::privacy::set_noise_seed(noise_injector, 12345) // 固定种子以确保可重现性
  
  // 测试数据
  let numeric_values = [
    ("metric1", 100.0),
    ("metric2", 250.5),
    ("metric3", 75.25),
    ("metric4", 500.0),
    ("metric5", 125.75)
  ]
  
  // 注入噪声
  let noisy_values = []
  for (metric_name, value) in numeric_values {
    let noisy_value = azimuth::privacy::inject_noise(noise_injector, value)
    noisy_values = noisy_values.push((metric_name, value, noisy_value))
  }
  
  // 验证噪声注入结果
  for (metric_name, original, noisy) in noisy_values {
    assert_not_eq(original, noisy) // 注入噪声后的值应该不同
    
    // 验证噪声在合理范围内（±3个尺度参数）
    let noise_difference = (noisy - original).abs()
    assert_true(noise_difference <= 3.0 * 1.0)
  }
  
  // 验证噪声一致性
  let test_value = 100.0
  let noisy1 = azimuth::privacy::inject_noise(noise_injector, test_value)
  let noisy2 = azimuth::privacy::inject_noise(noise_injector, test_value)
  
  // 使用相同种子应该产生相同结果
  assert_eq(noisy1, noisy2)
  
  // 验证噪声统计特性
  let noise_samples = []
  for i in 0..=999 {
    let noisy = azimuth::privacy::inject_noise(noise_injector, 0.0) // 注入到0值
    noise_samples = noise_samples.push(noisy)
  }
  
  // 计算噪声样本的均值和标准差
  let mean = noise_samples.reduce(fn(acc, x) { acc + x }, 0.0) / noise_samples.length().to_float()
  let variance = noise_samples.reduce(fn(acc, x) { acc + (x - mean) * (x - mean) }, 0.0) / noise_samples.length().to_float()
  let std_dev = variance.sqrt()
  
  // 拉普拉斯分布的均值应该接近0，标准差应该接近sqrt(2) * scale
  assert_true(mean.abs() <= 0.1) // 均值应该接近0
  assert_true(std_dev >= 1.3 && std_dev <= 1.5) // 标准差应该接近sqrt(2) ≈ 1.414
}

// Test 6: 差分隐私保护
test "differential privacy protection" {
  // 创建差分隐私处理器
  let dp_processor = azimuth::privacy::DifferentialPrivacy::new()
  
  // 配置差分隐私参数
  azimuth::privacy::set_epsilon(dp_processor, 1.0) // 隐私预算
  azimuth::privacy::set_delta(dp_processor, 0.00001) // 忽略概率
  azimuth::privacy::set_sensitivity(dp_processor, 1.0) // 查询敏感度
  
  // 创建数据集
  let dataset = []
  for i in 0..=999 {
    let value = 50.0 + (i % 100).to_float() // 值在50-149之间
    dataset = dataset.push(value)
  }
  
  // 计算原始统计量
  let original_mean = dataset.reduce(fn(acc, x) { acc + x }, 0.0) / dataset.length().to_float()
  let original_count = dataset.length().to_float()
  
  // 应用差分隐私
  let dp_mean = azimuth::privacy::dp_mean(dp_processor, dataset)
  let dp_count = azimuth::privacy::dp_count(dp_processor, dataset)
  
  // 验证差分隐私结果
  assert_not_eq(dp_mean, original_mean) // 差分隐私均值应该与原始均值不同
  assert_not_eq(dp_count, original_count) // 差分隐私计数应该与原始计数不同
  
  // 验证差分隐私结果在合理范围内
  assert_true(dp_mean >= 40.0 && dp_mean <= 160.0) // 均值应该在合理范围内
  assert_true(dp_count >= 900.0 && dp_count <= 1100.0) // 计数应该在合理范围内
  
  // 验证差分隐私的可组合性
  let epsilon1 = 0.5
  let epsilon2 = 0.5
  let combined_epsilon = azimuth::privacy::combine_epsilon(epsilon1, epsilon2)
  assert_eq(combined_epsilon, 1.0) // 组合隐私预算应该是各部分之和
  
  // 验证隐私预算消耗
  let initial_budget = azimuth::privacy::get_remaining_budget(dp_processor)
  azimuth::privacy::dp_mean(dp_processor, dataset) // 消耗隐私预算
  let remaining_budget = azimuth::privacy::get_remaining_budget(dp_processor)
  
  assert_true(remaining_budget < initial_budget) // 隐私预算应该减少
}

// Test 7: 隐私策略执行
test "privacy policy enforcement" {
  // 创建隐私策略管理器
  let policy_manager = azimuth::privacy::PolicyManager::new()
  
  // 创建隐私策略
  let policy = azimuth::privacy::Policy::new("GDPR Compliance")
  
  // 添加策略规则
  azimuth::privacy::policy::add_rule(policy, "personal_data", "must_anonymize", true)
  azimuth::privacy::policy::add_rule(policy, "email", "anonymization_method", "tokenization")
  azimuth::privacy::policy::add_rule(policy, "ip_address", "retention_days", 30)
  azimuth::privacy::policy::add_rule(policy, "location_data", "generalization_level", "city")
  azimuth::privacy::policy::add_rule(policy, "user_id", "pseudonymization", true)
  
  // 注册策略
  azimuth::privacy::register_policy(policy_manager, policy)
  
  // 创建测试数据
  let test_record = azimuth::privacy::Record::new()
  azimuth::privacy::record::add_field(test_record, "user_id", "user123")
  azimuth::privacy::record::add_field(test_record, "email", "user@example.com")
  azimuth::privacy::record::add_field(test_record, "ip_address", "192.168.1.1")
  azimuth::privacy::record::add_field(test_record, "location", "123 Main St, Springfield, IL, USA")
  azimuth::privacy::record::add_field(test_record, "age", "35")
  azimuth::privacy::record::add_field(test_record, "non_personal", "some data")
  
  // 应用隐私策略
  let compliance_result = azimuth::privacy::apply_policy(policy_manager, test_record)
  
  // 验证合规性
  assert_true(azimuth::privacy::is_compliant(compliance_result))
  
  // 获取处理后的记录
  let processed_record = azimuth::privacy::get_processed_record(compliance_result)
  
  // 验证字段处理
  let user_id = azimuth::privacy::record::get_field(processed_record, "user_id")
  let email = azimuth::privacy::record::get_field(processed_record, "email")
  let ip_address = azimuth::privacy::record::get_field(processed_record, "ip_address")
  let location = azimuth::privacy::record::get_field(processed_record, "location")
  let age = azimuth::privacy::record::get_field(processed_record, "age")
  let non_personal = azimuth::privacy::record::get_field(processed_record, "non_personal")
  
  // 验证处理结果
  assert_not_eq(user_id, "user123") // 用户ID应该被伪匿名化
  assert_not_eq(email, "user@example.com") // 邮箱应该被令牌化
  assert_eq(ip_address, "192.168.0.0") // IP地址应该被泛化
  assert_eq(location, "Springfield, IL, USA") // 位置应该被泛化到城市级别
  assert_eq(age, "30-40") // 年龄应该被泛化
  assert_eq(non_personal, "some data") // 非个人数据应该保持不变
  
  // 获取合规性报告
  let compliance_report = azimuth::privacy::get_compliance_report(compliance_result)
  let total_fields = azimuth::privacy::report::get_total_fields(compliance_report)
  let processed_fields = azimuth::privacy::report::get_processed_fields(compliance_report)
  let compliance_score = azimuth::privacy::report::get_compliance_score(compliance_report)
  
  assert_eq(total_fields, 6)
  assert_eq(processed_fields, 5) // 除了non_personal字段外都应被处理
  assert_eq(compliance_score, 100.0) // 合规性分数应该是100%
}

// Test 8: 隐私影响评估
test "privacy impact assessment" {
  // 创建隐私影响评估器
  let assessor = azimuth::privacy::ImpactAssessor::new()
  
  // 配置评估标准
  azimuth::privacy::add_assessment_criterion(assessor, "data_sensitivity", 0.3) // 数据敏感性权重30%
  azimuth::privacy::add_assessment_criterion(assessor, "processing_purpose", 0.2) // 处理目的权重20%
  azimuth::privacy::add_assessment_criterion(assessor, "data_retention", 0.2) // 数据保留权重20%
  azimuth::privacy::add_assessment_criterion(assessor, "access_controls", 0.15) // 访问控制权重15%
  azimuth::privacy::add_assessment_criterion(assessor, "encryption_level", 0.15) // 加密级别权重15%
  
  // 创建数据处理场景
  let scenario1 = azimuth::privacy::Scenario::new("User Analytics")
  azimuth::privacy::scenario::set_data_sensitivity(scenario1, "medium") // 中等敏感度
  azimuth::privacy::scenario::set_processing_purpose(scenario1, "analytics") // 分析目的
  azimuth::privacy::scenario::set_data_retention(scenario1, 90) // 保留90天
  azimuth::privacy::scenario::set_access_controls(scenario1, "role_based") // 基于角色的访问控制
  azimuth::privacy::scenario::set_encryption_level(scenario1, "aes256") // AES-256加密
  
  let scenario2 = azimuth::privacy::Scenario::new("Marketing Campaign")
  azimuth::privacy::scenario::set_data_sensitivity(scenario2, "high") // 高敏感度
  azimuth::privacy::scenario::set_processing_purpose(scenario2, "marketing") // 营销目的
  azimuth::privacy::scenario::set_data_retention(scenario2, 365) // 保留365天
  azimuth::privacy::scenario::set_access_controls(scenario2, "basic") // 基本访问控制
  azimuth::privacy::scenario::set_encryption_level(scenario2, "aes128") // AES-128加密
  
  // 执行隐私影响评估
  let assessment1 = azimuth::privacy::assess_impact(assessor, scenario1)
  let assessment2 = azimuth::privacy::assess_impact(assessor, scenario2)
  
  // 获取评估结果
  let risk_level1 = azimuth::privacy::assessment::get_risk_level(assessment1)
  let risk_score1 = azimuth::privacy::assessment::get_risk_score(assessment1)
  let recommendations1 = azimuth::privacy::assessment::get_recommendations(assessment1)
  
  let risk_level2 = azimuth::privacy::assessment::get_risk_level(assessment2)
  let risk_score2 = azimuth::privacy::assessment::get_risk_score(assessment2)
  let recommendations2 = azimuth::privacy::assessment::get_recommendations(assessment2)
  
  // 验证评估结果
  assert_eq(risk_level1, "low") // 场景1应该是低风险
  assert_true(risk_score1 < 0.4) // 风险分数应该小于40%
  
  assert_eq(risk_level2, "medium") // 场景2应该是中等风险
  assert_true(risk_score2 >= 0.4 && risk_score2 < 0.7) // 风险分数应该在40-70%之间
  
  assert_true(recommendations1.length() < recommendations2.length()) // 场景2应该有更多建议
  
  // 验证建议内容
  let has_encryption_recommendation = recommendations2.any(fn(r) { r.contains("encryption") })
  let has_retention_recommendation = recommendations2.any(fn(r) { r.contains("retention") })
  
  assert_true(has_encryption_recommendation) // 应该有加密相关建议
  assert_true(has_retention_recommendation) // 应该有数据保留相关建议
  
  // 生成隐私影响评估报告
  let report = azimuth::privacy::generate_impact_report(assessor, [scenario1, scenario2])
  
  // 验证报告内容
  let overall_risk_level = azimuth::privacy::report::get_overall_risk_level(report)
  let total_recommendations = azimuth::privacy::report::get_total_recommendations(report)
  
  assert_eq(overall_risk_level, "medium") // 整体风险应该是中等
  assert_true(total_recommendations > 0) // 应该有建议
}