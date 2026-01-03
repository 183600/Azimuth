// Azimuth 遥测数据国际化支持测试用例
// 专注于遥测系统中的国际化支持和多语言处理机制

// 测试1: 多语言错误消息处理
test "多语言错误消息处理" {
  // 模拟多语言错误消息数据
  let error_messages = [
    { 
      error_code: "AUTH_FAILED", 
      language: "en", 
      message: "Authentication failed",
      severity: "error",
      context: { "service": "auth-service", "user_id": "user123" }
    },
    { 
      error_code: "AUTH_FAILED", 
      language: "zh", 
      message: "认证失败",
      severity: "error",
      context: { "service": "auth-service", "user_id": "user123" }
    },
    { 
      error_code: "AUTH_FAILED", 
      language: "ja", 
      message: "認証に失敗しました",
      severity: "error",
      context: { "service": "auth-service", "user_id": "user123" }
    },
    { 
      error_code: "RATE_LIMIT_EXCEEDED", 
      language: "en", 
      message: "Rate limit exceeded",
      severity: "warning",
      context: { "service": "api-gateway", "ip": "192.168.1.1" }
    },
    { 
      error_code: "RATE_LIMIT_EXCEEDED", 
      language: "zh", 
      message: "请求频率超限",
      severity: "warning",
      context: { "service": "api-gateway", "ip": "192.168.1.1" }
    },
    { 
      error_code: "RATE_LIMIT_EXCEEDED", 
      language: "ja", 
      message: "レート制限を超えました",
      severity: "warning",
      context: { "service": "api-gateway", "ip": "192.168.1.1" }
    },
    { 
      error_code: "DATA_VALIDATION_ERROR", 
      language: "en", 
      message: "Data validation error",
      severity: "error",
      context: { "service": "user-service", "field": "email" }
    },
    { 
      error_code: "DATA_VALIDATION_ERROR", 
      language: "zh", 
      message: "数据验证错误",
      severity: "error",
      context: { "service": "user-service", "field": "email" }
    }
  ]
  
  // 国际化配置
  let i18n_config = {
    default_language: "en",
    supported_languages: ["en", "zh", "ja", "fr", "de"],
    fallback_strategy: "default_language", // 回退策略
    context_preservation: true,           // 保留上下文
    severity_translation: true            // 严重级别翻译
  }
  
  // 多语言错误消息处理算法
  let mut i18n_results = []
  
  // 按错误代码分组
  let mut error_groups = {}
  for error in error_messages {
    let group = error_groups.get(error.error_code).unwrap_or([])
    error_groups = error_groups.set(error.error_code, group.push(error))
  }
  
  // 处理每个错误代码的多语言消息
  for (error_code, messages) in error_groups {
    // 检查语言覆盖
    let languages = messages.map(fn(m) { m.language }).to_set()
    let missing_languages = []
    
    for lang in i18n_config.supported_languages {
      if not languages.contains(lang) {
        missing_languages = missing_languages.push(lang)
      }
    }
    
    // 验证上下文一致性
    let mut context_consistency = true
    let reference_context = messages[0].context
    
    for message in messages {
      for (key, value) in message.context {
        match reference_context.get(key) {
          Some(ref_value) => {
            if ref_value != value {
              context_consistency = false
              break
            }
          }
          None => {
            context_consistency = false
            break
          }
        }
      }
    }
    
    // 验证严重级别一致性
    let severities = messages.map(fn(m) { m.severity }).to_set()
    let severity_consistent = severities.size() == 1
    
    // 计算翻译完整性得分
    let translation_completeness = languages.size().to_float() / i18n_config.supported_languages.length().to_float() * 100.0
    
    // 确定翻译质量
    let translation_quality = 
      if translation_completeness >= 80.0 && context_consistency && severity_consistent {
        "high"
      } else if translation_completeness >= 60.0 {
        "medium"
      } else {
        "low"
      }
    
    // 生成翻译建议
    let translation_recommendations = []
    
    if missing_languages.length() > 0 {
      translation_recommendations = translation_recommendations.push({
        action: "add_translations",
        details: "Add translations for missing languages: " + missing_languages.join(", ")
      })
    }
    
    if not context_consistency {
      translation_recommendations = translation_recommendations.push({
        action: "fix_context",
        details: "Ensure context consistency across all languages"
      })
    }
    
    if not severity_consistent {
      translation_recommendations = translation_recommendations.push({
        action: "fix_severity",
        details: "Ensure severity level consistency across all languages"
      })
    }
    
    i18n_results = i18n_results.push({
      error_code: error_code,
      supported_languages: languages.to_array(),
      missing_languages: missing_languages,
      translation_completeness: translation_completeness,
      context_consistency: context_consistency,
      severity_consistent: severity_consistent,
      translation_quality: translation_quality,
      translation_recommendations: translation_recommendations
    })
  }
  
  // 验证多语言错误消息处理结果
  assert_eq(i18n_results.length(), 3) // 应该有3个错误代码
  
  // 验证AUTH_FAILED错误处理
  let auth_failed_result = i18n_results.filter(fn(r) { r.error_code == "AUTH_FAILED" })[0]
  assert_eq(auth_failed_result.supported_languages.length(), 3) // 支持en, zh, ja
  assert_true(auth_failed_result.translation_completeness >= 60.0) // 至少60%完整
  assert_true(auth_failed_result.context_consistency) // 上下文应该一致
  assert_true(auth_failed_result.severity_consistent) // 严重级别应该一致
  
  // 验证RATE_LIMIT_EXCEEDED错误处理
  let rate_limit_result = i18n_results.filter(fn(r) { r.error_code == "RATE_LIMIT_EXCEEDED" })[0]
  assert_eq(rate_limit_result.supported_languages.length(), 3) // 支持en, zh, ja
  assert_true(rate_limit_result.translation_completeness >= 60.0) // 至少60%完整
  assert_true(rate_limit_result.context_consistency) // 上下文应该一致
  assert_true(rate_limit_result.severity_consistent) // 严重级别应该一致
  
  // 验证DATA_VALIDATION_ERROR错误处理
  let validation_result = i18n_results.filter(fn(r) { r.error_code == "DATA_VALIDATION_ERROR" })[0]
  assert_eq(validation_result.supported_languages.length(), 2) // 支持en, zh
  assert_true(validation_result.translation_completeness < 60.0) // 完整性应该低于60%
  assert_true(validation_result.missing_languages.length() > 0) // 应该有缺失语言
  assert_true(validation_result.missing_languages.contains("ja"))
}

// 测试2: 本地化时间格式处理
test "本地化时间格式处理" {
  // 模拟不同地区的时间格式数据
  let time_format_data = [
    { 
      timestamp: 1640995200, 
      locale: "en-US", 
      timezone: "America/New_York",
      format: "MM/DD/YYYY HH:mm:ss",
      expected: "01/01/2022 00:00:00"
    },
    { 
      timestamp: 1640995200, 
      locale: "zh-CN", 
      timezone: "Asia/Shanghai",
      format: "YYYY年MM月DD日 HH:mm:ss",
      expected: "2022年01月01日 13:00:00"
    },
    { 
      timestamp: 1640995200, 
      locale: "ja-JP", 
      timezone: "Asia/Tokyo",
      format: "YYYY/MM/DD HH:mm:ss",
      expected: "2022/01/01 14:00:00"
    },
    { 
      timestamp: 1640995200, 
      locale: "de-DE", 
      timezone: "Europe/Berlin",
      format: "DD.MM.YYYY HH:mm:ss",
      expected: "01.01.2022 06:00:00"
    },
    { 
      timestamp: 1640995200, 
      locale: "fr-FR", 
      timezone: "Europe/Paris",
      format: "DD/MM/YYYY HH:mm:ss",
      expected: "01/01/2022 06:00:00"
    }
  ]
  
  // 时区偏移量（相对于UTC的秒数）
  let timezone_offsets = {
    "America/New_York": -18000,  // UTC-5
    "Asia/Shanghai": 28800,     // UTC+8
    "Asia/Tokyo": 32400,        // UTC+9
    "Europe/Berlin": 3600,      // UTC+1
    "Europe/Paris": 3600        // UTC+1
  }
  
  // 本地化时间格式处理算法
  let mut localized_time_results = []
  
  for time_data in time_format_data {
    // 获取时区偏移
    let timezone_offset = timezone_offsets.get(time_data.timezone).unwrap()
    
    // 计算本地时间戳
    let local_timestamp = time_data.timestamp + timezone_offset
    
    // 转换为日期时间组件（简化计算）
    let utc_date = 1640995200 // 2022-01-01 00:00:00 UTC
    let days_since_epoch = local_timestamp / 86400
    let seconds_in_day = local_timestamp % 86400
    
    let year = 2022
    let month = 1
    let day = (days_since_epoch % 31) + 1
    let hour = seconds_in_day / 3600
    let minute = (seconds_in_day % 3600) / 60
    let second = seconds_in_day % 60
    
    // 根据地区格式化时间
    let formatted_time = match time_data.locale {
      "en-US" => {
        let mm = if month < 10 { "0" + month.to_string() } else { month.to_string() }
        let dd = if day < 10 { "0" + day.to_string() } else { day.to_string() }
        let hh = if hour < 10 { "0" + hour.to_string() } else { hour.to_string() }
        let min = if minute < 10 { "0" + minute.to_string() } else { minute.to_string() }
        let ss = if second < 10 { "0" + second.to_string() } else { second.to_string() }
        mm + "/" + dd + "/2022 " + hh + ":" + min + ":" + ss
      }
      "zh-CN" => {
        let mm = if month < 10 { "0" + month.to_string() } else { month.to_string() }
        let dd = if day < 10 { "0" + day.to_string() } else { day.to_string() }
        let hh = if hour < 10 { "0" + hour.to_string() } else { hour.to_string() }
        let min = if minute < 10 { "0" + minute.to_string() } else { minute.to_string() }
        let ss = if second < 10 { "0" + second.to_string() } else { second.to_string() }
        "2022年" + mm + "月" + dd + "日 " + hh + ":" + min + ":" + ss
      }
      "ja-JP" => {
        let mm = if month < 10 { "0" + month.to_string() } else { month.to_string() }
        let dd = if day < 10 { "0" + day.to_string() } else { day.to_string() }
        let hh = if hour < 10 { "0" + hour.to_string() } else { hour.to_string() }
        let min = if minute < 10 { "0" + minute.to_string() } else { minute.to_string() }
        let ss = if second < 10 { "0" + second.to_string() } else { second.to_string() }
        "2022/" + mm + "/" + dd + " " + hh + ":" + min + ":" + ss
      }
      "de-DE" => {
        let mm = if month < 10 { "0" + month.to_string() } else { month.to_string() }
        let dd = if day < 10 { "0" + day.to_string() } else { day.to_string() }
        let hh = if hour < 10 { "0" + hour.to_string() } else { hour.to_string() }
        let min = if minute < 10 { "0" + minute.to_string() } else { minute.to_string() }
        let ss = if second < 10 { "0" + second.to_string() } else { second.to_string() }
        dd + "." + mm + ".2022 " + hh + ":" + min + ":" + ss
      }
      "fr-FR" => {
        let mm = if month < 10 { "0" + month.to_string() } else { month.to_string() }
        let dd = if day < 10 { "0" + day.to_string() } else { day.to_string() }
        let hh = if hour < 10 { "0" + hour.to_string() } else { hour.to_string() }
        let min = if minute < 10 { "0" + minute.to_string() } else { minute.to_string() }
        let ss = if second < 10 { "0" + second.to_string() } else { second.to_string() }
        dd + "/" + mm + "/2022 " + hh + ":" + min + ":" + ss
      }
      _ => "Unknown format"
    }
    
    // 验证格式化结果
    let is_format_correct = formatted_time == time_data.expected
    
    localized_time_results = localized_time_results.push({
      locale: time_data.locale,
      timezone: time_data.timezone,
      original_timestamp: time_data.timestamp,
      local_timestamp: local_timestamp,
      formatted_time: formatted_time,
      expected_time: time_data.expected,
      is_format_correct: is_format_correct
    })
  }
  
  // 验证本地化时间格式处理结果
  assert_eq(localized_time_results.length(), 5)
  
  // 验证所有时间格式都正确
  for result in localized_time_results {
    assert_true(result.is_format_correct) // 所有格式都应该正确
  }
  
  // 验证时区转换
  let us_result = localized_time_results.filter(fn(r) { r.locale == "en-US" })[0]
  assert_eq(us_result.local_timestamp, 1640977200) // UTC-5
  
  let cn_result = localized_time_results.filter(fn(r) { r.locale == "zh-CN" })[0]
  assert_eq(cn_result.local_timestamp, 1641024000) // UTC+8
  
  let jp_result = localized_time_results.filter(fn(r) { r.locale == "ja-JP" })[0]
  assert_eq(jp_result.local_timestamp, 1641027600) // UTC+9
  
  // 验证不同地区的时间表示
  assert_eq(us_result.formatted_time, "01/01/2022 00:00:00") // MM/DD/YYYY
  assert_eq(cn_result.formatted_time, "2022年01月01日 13:00:00") // YYYY年MM月DD日
  assert_eq(jp_result.formatted_time, "2022/01/01 14:00:00") // YYYY/MM/DD
}

// 测试3: 多语言数字和单位格式化
test "多语言数字和单位格式化" {
  // 模拟多语言数字和单位数据
  let number_format_data = [
    { 
      value: 1234.567, 
      locale: "en-US", 
      type: "decimal",
      expected: "1,234.567"
    },
    { 
      value: 1234.567, 
      locale: "de-DE", 
      type: "decimal",
      expected: "1.234,567"
    },
    { 
      value: 1234.567, 
      locale: "fr-FR", 
      type: "decimal",
      expected: "1 234,567"
    },
    { 
      value: 9876543, 
      locale: "en-US", 
      type: "integer",
      expected: "9,876,543"
    },
    { 
      value: 9876543, 
      locale: "zh-CN", 
      type: "integer",
      expected: "9,876,543"
    },
    { 
      value: 0.8547, 
      locale: "en-US", 
      type: "percentage",
      expected: "85.47%"
    },
    { 
      value: 0.8547, 
      locale: "de-DE", 
      type: "percentage",
      expected: "85,47%"
    },
    { 
      value: 1024.5, 
      locale: "en-US", 
      type: "bytes",
      expected: "1.00 KB"
    },
    { 
      value: 1024.5, 
      locale: "zh-CN", 
      type: "bytes",
      expected: "1.00 KB"
    },
    { 
      value: 5000, 
      locale: "en-US", 
      type: "currency",
      currency: "USD",
      expected: "$5,000.00"
    },
    { 
      value: 5000, 
      locale: "de-DE", 
      type: "currency",
      currency: "EUR",
      expected: "5.000,00 €"
    },
    { 
      value: 5000, 
      locale: "ja-JP", 
      type: "currency",
      currency: "JPY",
      expected: "¥5,000"
    }
  ]
  
  // 多语言数字格式化算法
  let mut formatted_number_results = []
  
  for number_data in number_format_data {
    // 根据地区和类型格式化数字
    let formatted_number = match (number_data.locale, number_data.type) {
      // 小数格式化
      ("en-US", "decimal") => {
        let int_part = number_data.value.to_int().to_string()
        let decimal_part = ((number_data.value - number_data.value.to_int().to_float()) * 1000.0).to_int().to_string()
        int_part + "." + decimal_part
      }
      ("de-DE", "decimal") => {
        let int_part = number_data.value.to_int().to_string()
        let decimal_part = ((number_data.value - number_data.value.to_int().to_float()) * 1000.0).to_int().to_string()
        int_part.replace(",", ".") + "," + decimal_part
      }
      ("fr-FR", "decimal") => {
        let int_part = number_data.value.to_int().to_string()
        let decimal_part = ((number_data.value - number_data.value.to_int().to_float()) * 1000.0).to_int().to_string()
        int_part.replace(",", " ") + "," + decimal_part
      }
      
      // 整数格式化
      ("en-US", "integer") => {
        let int_str = number_data.value.to_int().to_string()
        int_str.replace(/(\d)(?=(\d{3})+$)/g, "$1,")
      }
      ("zh-CN", "integer") => {
        let int_str = number_data.value.to_int().to_string()
        int_str.replace(/(\d)(?=(\d{3})+$)/g, "$1,")
      }
      
      // 百分比格式化
      ("en-US", "percentage") => {
        let percentage = (number_data.value * 100.0).to_string()
        percentage.replace(/(\d+\.\d{2}).*/, "$1") + "%"
      }
      ("de-DE", "percentage") => {
        let percentage = (number_data.value * 100.0).to_string()
        percentage.replace(".", ",").replace(/(\d+,\d{2}).*/, "$1") + "%"
      }
      
      // 字节格式化
      ("en-US", "bytes") | ("zh-CN", "bytes") => {
        if number_data.value >= 1024.0 {
          let kb = number_data.value / 1024.0
          kb.to_string().replace(/(\d+\.\d{2}).*/, "$1") + " KB"
        } else {
          number_data.value.to_string() + " B"
        }
      }
      
      // 货币格式化
      ("en-US", "currency") => {
        let currency = match number_data.currency {
          "USD" => "$",
          _ => "$"
        }
        let amount = number_data.value.to_string()
        currency + amount.replace(/(\d)(?=(\d{3})+$)/g, "$1,") + ".00"
      }
      ("de-DE", "currency") => {
        let currency = match number_data.currency {
          "EUR" => "€",
          _ => "€"
        }
        let amount = number_data.value.to_string()
        amount.replace(/(\d)(?=(\d{3})+$)/g, "$1.") + ",00 " + currency
      }
      ("ja-JP", "currency") => {
        let currency = match number_data.currency {
          "JPY" => "¥",
          _ => "¥"
        }
        let amount = number_data.value.to_int().to_string()
        currency + amount.replace(/(\d)(?=(\d{3})+$)/g, "$1,")
      }
      
      _ => "Unknown format"
    }
    
    // 验证格式化结果
    let is_format_correct = formatted_number == number_data.expected
    
    formatted_number_results = formatted_number_results.push({
      locale: number_data.locale,
      type: number_data.type,
      original_value: number_data.value,
      formatted_number: formatted_number,
      expected_format: number_data.expected,
      is_format_correct: is_format_correct
    })
  }
  
  // 验证多语言数字格式化结果
  assert_eq(formatted_number_results.length(), 12)
  
  // 验证小数格式化
  let decimal_formats = formatted_number_results.filter(fn(r) { r.type == "decimal" })
  for result in decimal_formats {
    assert_true(result.is_format_correct) // 所有小数格式都应该正确
  }
  
  // 验证整数格式化
  let integer_formats = formatted_number_results.filter(fn(r) { r.type == "integer" })
  for result in integer_formats {
    assert_true(result.is_format_correct) // 所有整数格式都应该正确
  }
  
  // 验证百分比格式化
  let percentage_formats = formatted_number_results.filter(fn(r) { r.type == "percentage" })
  for result in percentage_formats {
    assert_true(result.is_format_correct) // 所有百分比格式都应该正确
  }
  
  // 验证字节格式化
  let byte_formats = formatted_number_results.filter(fn(r) { r.type == "bytes" })
  for result in byte_formats {
    assert_true(result.is_format_correct) // 所有字节格式都应该正确
  }
  
  // 验证货币格式化
  let currency_formats = formatted_number_results.filter(fn(r) { r.type == "currency" })
  for result in currency_formats {
    assert_true(result.is_format_correct) // 所有货币格式都应该正确
  }
  
  // 验证不同地区的分隔符
  let us_decimal = formatted_number_results.filter(fn(r) { r.locale == "en-US" && r.type == "decimal" })[0]
  assert_true(us_decimal.formatted_number.contains(".")) // 美国使用小数点
  
  let de_decimal = formatted_number_results.filter(fn(r) { r.locale == "de-DE" && r.type == "decimal" })[0]
  assert_true(de_decimal.formatted_number.contains(",")) // 德国使用逗号作为小数点
}

// 测试4: 多语言度量单位转换
test "多语言度量单位转换" {
  // 模拟多语言度量单位数据
  let unit_conversion_data = [
    { 
      value: 1000.0, 
      from_unit: "meter", 
      to_unit: "kilometer", 
      locale: "en-US",
      expected: "1.0 km"
    },
    { 
      value: 1000.0, 
      from_unit: "meter", 
      to_unit: "kilometer", 
      locale: "zh-CN",
      expected: "1.0 公里"
    },
    { 
      value: 1000.0, 
      from_unit: "gram", 
      to_unit: "kilogram", 
      locale: "en-US",
      expected: "1.0 kg"
    },
    { 
      value: 1000.0, 
      from_unit: "gram", 
      to_unit: "kilogram", 
      locale: "ja-JP",
      expected: "1.0 キログラム"
    },
    { 
      value: 3600.0, 
      from_unit: "second", 
      to_unit: "hour", 
      locale: "en-US",
      expected: "1.0 hour"
    },
    { 
      value: 3600.0, 
      from_unit: "second", 
      to_unit: "hour", 
      locale: "de-DE",
      expected: "1.0 Stunde"
    },
    { 
      value: 1000.0, 
      from_unit: "byte", 
      to_unit: "kilobyte", 
      locale: "en-US",
      expected: "1.0 KB"
    },
    { 
      value: 1000.0, 
      from_unit: "byte", 
      to_unit: "kilobyte", 
      locale: "fr-FR",
      expected: "1.0 Ko"
    },
    { 
      value: 100.0, 
      from_unit: "celsius", 
      to_unit: "fahrenheit", 
      locale: "en-US",
      expected: "212.0 °F"
    },
    { 
      value: 100.0, 
      from_unit: "celsius", 
      to_unit: "fahrenheit", 
      locale: "zh-CN",
      expected: "212.0 华氏度"
    }
  ]
  
  // 单位转换因子
  let conversion_factors = {
    "meter:kilometer": 0.001,
    "gram:kilogram": 0.001,
    "second:hour": 0.0002777777777777778,
    "byte:kilobyte": 0.001,
    "celsius:fahrenheit": (9.0/5.0, 32.0) // 特殊情况：斜率和截距
  }
  
  // 多语言单位名称
  let unit_names = {
    "en-US": {
      "kilometer": "km",
      "kilogram": "kg",
      "hour": "hour",
      "kilobyte": "KB",
      "fahrenheit": "°F"
    },
    "zh-CN": {
      "kilometer": "公里",
      "kilogram": "千克",
      "hour": "小时",
      "kilobyte": "KB",
      "fahrenheit": "华氏度"
    },
    "ja-JP": {
      "kilometer": "キロメートル",
      "kilogram": "キログラム",
      "hour": "時間",
      "kilobyte": "KB",
      "fahrenheit": "華氏"
    },
    "de-DE": {
      "kilometer": "km",
      "kilogram": "kg",
      "hour": "Stunde",
      "kilobyte": "KB",
      "fahrenheit": "°F"
    },
    "fr-FR": {
      "kilometer": "km",
      "kilogram": "kg",
      "hour": "heure",
      "kilobyte": "Ko",
      "fahrenheit": "°F"
    }
  }
  
  // 多语言度量单位转换算法
  let mut unit_conversion_results = []
  
  for conversion_data in unit_conversion_data {
    // 获取转换因子
    let conversion_key = conversion_data.from_unit + ":" + conversion_data.to_unit
    let converted_value = match conversion_key {
      "celsius:fahrenheit" => {
        let (slope, intercept) = conversion_factors.get(conversion_key).unwrap()
        conversion_data.value * slope + intercept.0
      }
      _ => {
        let factor = conversion_factors.get(conversion_key).unwrap()
        conversion_data.value * factor
      }
    }
    
    // 获取本地化单位名称
    let locale_units = unit_names.get(conversion_data.locale).unwrap()
    let localized_unit = locale_units.get(conversion_data.to_unit).unwrap()
    
    // 格式化结果
    let formatted_result = converted_value.to_string().replace(/(\d+\.\d{1}).*/, "$1") + " " + localized_unit
    
    // 验证转换结果
    let is_conversion_correct = formatted_result == conversion_data.expected
    
    unit_conversion_results = unit_conversion_results.push({
      locale: conversion_data.locale,
      from_unit: conversion_data.from_unit,
      to_unit: conversion_data.to_unit,
      original_value: conversion_data.value,
      converted_value: converted_value,
      localized_unit: localized_unit,
      formatted_result: formatted_result,
      expected_result: conversion_data.expected,
      is_conversion_correct: is_conversion_correct
    })
  }
  
  // 验证多语言度量单位转换结果
  assert_eq(unit_conversion_results.length(), 10)
  
  // 验证所有转换都正确
  for result in unit_conversion_results {
    assert_true(result.is_conversion_correct) // 所有转换都应该正确
  }
  
  // 验证不同语言的单位名称
  let us_kilometer = unit_conversion_results.filter(fn(r) { 
    r.locale == "en-US" && r.to_unit == "kilometer" 
  })[0]
  assert_eq(us_kilometer.localized_unit, "km")
  
  let cn_kilometer = unit_conversion_results.filter(fn(r) { 
    r.locale == "zh-CN" && r.to_unit == "kilometer" 
  })[0]
  assert_eq(cn_kilometer.localized_unit, "公里")
  
  let jp_kilogram = unit_conversion_results.filter(fn(r) { 
    r.locale == "ja-JP" && r.to_unit == "kilogram" 
  })[0]
  assert_eq(jp_kilogram.localized_unit, "キログラム")
  
  // 验证特殊转换（摄氏度到华氏度）
  let us_fahrenheit = unit_conversion_results.filter(fn(r) { 
    r.locale == "en-US" && r.to_unit == "fahrenheit" 
  })[0]
  assert_eq(us_fahrenheit.converted_value, 212.0) // 100°C = 212°F
  
  let cn_fahrenheit = unit_conversion_results.filter(fn(r) { 
    r.locale == "zh-CN" && r.to_unit == "fahrenheit" 
  })[0]
  assert_eq(cn_fahrenheit.localized_unit, "华氏度")
}

// 测试5: 多语言用户界面适配
test "多语言用户界面适配" {
  // 模拟多语言UI元素数据
  let ui_elements = [
    { 
      element_id: "dashboard_title", 
      locale: "en", 
      text: "Telemetry Dashboard",
      direction: "ltr",
      text_length: 19
    },
    { 
      element_id: "dashboard_title", 
      locale: "zh", 
      text: "遥测仪表板",
      direction: "ltr",
      text_length: 5
    },
    { 
      element_id: "dashboard_title", 
      locale: "ar", 
      text: "لوحة القياس عن بعد",
      direction: "rtl",
      text_length: 18
    },
    { 
      element_id: "error_message", 
      locale: "en", 
      text: "Connection failed",
      direction: "ltr",
      text_length: 16
    },
    { 
      element_id: "error_message", 
      locale: "ja", 
      text: "接続に失敗しました",
      direction: "ltr",
      text_length: 10
    },
    { 
      element_id: "settings_button", 
      locale: "en", 
      text: "Settings",
      direction: "ltr",
      text_length: 8
    },
    { 
      element_id: "settings_button", 
      locale: "de", 
      text: "Einstellungen",
      direction: "ltr",
      text_length: 13
    },
    { 
      element_id: "date_range_label", 
      locale: "en", 
      text: "Date Range:",
      direction: "ltr",
      text_length: 11
    },
    { 
      element_id: "date_range_label", 
      locale: "he", 
      text: "טווח תאריכים:",
      direction: "rtl",
      text_length: 12
    }
  ]
  
  // UI适配配置
  let ui_adaptation_config = {
    max_text_length_ratio: 2.0,   // 最大文本长度比例
    rtl_languages: ["ar", "he", "fa", "ur"], // 从右到左的语言
    font_families: {
      "en": "Arial, sans-serif",
      "zh": "PingFang SC, sans-serif",
      "ja": "Hiragino Sans, sans-serif",
      "ar": "Tahoma, sans-serif",
      "de": "Roboto, sans-serif",
      "he": "Arial Hebrew, sans-serif"
    },
    text_adjustments: {
      "zh": { "font_size": "1.1em" },
      "ja": { "font_size": "1.0em" },
      "ar": { "font_size": "1.2em" },
      "he": { "font_size": "1.1em" }
    }
  }
  
  // 多语言UI适配算法
  let mut ui_adaptation_results = []
  
  // 按元素ID分组
  let mut element_groups = {}
  for element in ui_elements {
    let group = element_groups.get(element.element_id).unwrap_or([])
    element_groups = element_groups.set(element.element_id, group.push(element))
  }
  
  // 处理每个UI元素的适配
  for (element_id, elements) in element_groups {
    // 获取基准文本（英文）
    let base_element = elements.filter(fn(e) { e.locale == "en" })[0]
    let base_length = base_element.text_length
    
    // 处理每种语言的适配
    for element in elements {
      // 计算文本长度比例
      let length_ratio = element.text_length.to_float() / base_length.to_float()
      let needs_layout_adjustment = length_ratio > ui_adaptation_config.max_text_length_ratio
      
      // 确定文本方向
      let is_rtl = ui_adaptation_config.rtl_languages.contains(element.locale)
      let text_direction = if is_rtl { "rtl" } else { "ltr" }
      
      // 获取推荐字体
      let recommended_font = ui_adaptation_config.font_families.get(element.locale).unwrap()
      
      // 获取文本调整建议
      let text_adjustments = ui_adaptation_config.text_adjustments.get(element.locale)
      
      // 计算UI布局建议
      let layout_recommendations = []
      
      if needs_layout_adjustment {
        layout_recommendations = layout_recommendations.push({
          adjustment: "expand_width",
          reason: "Text is significantly longer than base language"
        })
      }
      
      if is_rtl && element.direction != "rtl" {
        layout_recommendations = layout_recommendations.push({
          adjustment: "set_direction_rtl",
          reason: "Language requires right-to-left direction"
        })
      }
      
      if not is_rtl && element.direction == "rtl" {
        layout_recommendations = layout_recommendations.push({
          adjustment: "set_direction_ltr",
          reason: "Language requires left-to-right direction"
        })
      }
      
      ui_adaptation_results = ui_adaptation_results.push({
        element_id: element_id,
        locale: element.locale,
        text: element.text,
        text_direction: text_direction,
        recommended_font: recommended_font,
        text_adjustments: text_adjustments,
        length_ratio: length_ratio,
        needs_layout_adjustment: needs_layout_adjustment,
        layout_recommendations: layout_recommendations
      })
    }
  }
  
  // 验证多语言UI适配结果
  assert_eq(ui_adaptation_results.length(), 9)
  
  // 验证RTL语言适配
  let rtl_elements = ui_adaptation_results.filter(fn(r) { r.text_direction == "rtl" })
  assert_eq(rtl_elements.length(), 2) // 应该有2个RTL元素
  assert_true(rtl_elements.map(fn(r) { r.locale }).contains("ar"))
  assert_true(rtl_elements.map(fn(r) { r.locale }).contains("he"))
  
  // 验证字体推荐
  let zh_element = ui_adaptation_results.filter(fn(r) { r.locale == "zh" })[0]
  assert_eq(zh_element.recommended_font, "PingFang SC, sans-serif")
  assert_true(zh_element.text_adjustments.is_some()) // 中文应该有文本调整
  
  let ja_element = ui_adaptation_results.filter(fn(r) { r.locale == "ja" })[0]
  assert_eq(ja_element.recommended_font, "Hiragino Sans, sans-serif")
  
  let ar_element = ui_adaptation_results.filter(fn(r) { r.locale == "ar" })[0]
  assert_eq(ar_element.recommended_font, "Tahoma, sans-serif")
  assert_true(ar_element.text_adjustments.is_some()) // 阿拉伯语应该有文本调整
  
  // 验证布局建议
  let ar_title = ui_adaptation_results.filter(fn(r) { r.element_id == "dashboard_title" && r.locale == "ar" })[0]
  assert_true(ar_title.layout_recommendations.length() > 0) // 阿拉伯语标题应该有布局建议
  assert_true(ar_title.layout_recommendations.any(fn(r) { r.adjustment == "set_direction_rtl" }))
  
  // 验证文本长度比例
  let zh_title = ui_adaptation_results.filter(fn(r) { r.element_id == "dashboard_title" && r.locale == "zh" })[0]
  assert_eq(zh_title.length_ratio, 5.0 / 19.0) // 5/19
  assert_false(zh_title.needs_layout_adjustment) // 中文文本较短，不需要布局调整
  
  let de_settings = ui_adaptation_results.filter(fn(r) { r.element_id == "settings_button" && r.locale == "de" })[0]
  assert_eq(de_settings.length_ratio, 13.0 / 8.0) // 13/8
  assert_true(de_settings.needs_layout_adjustment) // 德语文本较长，需要布局调整
}