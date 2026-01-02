// Azimuth Telemetry System - 国际化全球化测试用例
// 测试系统在不同语言、地区和文化环境下的兼容性和正确性

// 测试1: 多语言字符集处理
test "多语言字符集处理测试" {
  // 测试各种语言的字符集
  let multilingual_strings = [
    ("中文", "Chinese"),
    ("日本語", "Japanese"),
    ("한국어", "Korean"),
    ("العربية", "Arabic"),
    ("עברית", "Hebrew"),
    ("Русский", "Russian"),
    ("Español", "Spanish"),
    ("Français", "French"),
    ("Deutsch", "German"),
    ("Português", "Portuguese"),
    ("Italiano", "Italian"),
    ("Nederlands", "Dutch"),
    ("Svenska", "Swedish"),
    ("Norsk", "Norwegian"),
    ("Dansk", "Danish"),
    ("Suomi", "Finnish"),
    ("Polski", "Polish"),
    ("Čeština", "Czech"),
    ("Magyar", "Hungarian"),
    ("Română", "Romanian"),
    ("Български", "Bulgarian"),
    ("Hrvatski", "Croatian"),
    ("Srpski", "Serbian"),
    ("Slovenčina", "Slovak"),
    ("Slovenščina", "Slovenian"),
    ("Ελληνικά", "Greek"),
    ("Türkçe", "Turkish"),
    ("Українська", "Ukrainian"),
    ("Беларуская", "Belarusian"),
    ("Қазақша", "Kazakh")
  ]
  
  // 验证多语言字符串处理
  for (native_name, english_name) in multilingual_strings {
    // 验证字符串不为空
    assert_true(native_name.length() > 0)
    assert_true(english_name.length() > 0)
    
    // 验证字符串可以正确转换大小写（如果适用）
    let native_upper = native_name.to_uppercase()
    let native_lower = native_name.to_lowercase()
    
    assert_true(native_upper.length() > 0)
    assert_true(native_lower.length() > 0)
    
    // 验证英文名称可以正确转换大小写
    let english_upper = english_name.to_uppercase()
    let english_lower = english_name.to_lowercase()
    
    assert_eq(english_upper, english_name.to_uppercase())
    assert_eq(english_lower, english_name.to_lowercase())
    
    // 验证字符串可以正确序列化和反序列化
    let serialized_native = native_name.to_string()
    let serialized_english = english_name.to_string()
    
    assert_eq(serialized_native, native_name)
    assert_eq(serialized_english, english_name)
  }
  
  // 测试Unicode字符处理
  let unicode_strings = [
    "🚀 Rocket",
    "🌟 Star",
    "💫 Comet",
    "✨ Sparkle",
    "🎯 Target",
    "📊 Chart",
    "📈 Trend",
    "📉 Decline",
    "🔍 Search",
    "⚙️ Settings",
    "🌐 Global",
    "🏢 Building",
    "👥 Team",
    "🔗 Link",
    "📝 Document"
  ]
  
  // 验证Unicode字符串处理
  for unicode_string in unicode_strings {
    assert_true(unicode_string.length() > 0)
    
    // 验证包含表情符号的字符串可以正确处理
    let serialized_unicode = unicode_string.to_string()
    assert_eq(serialized_unicode, unicode_string)
    
    // 验证字符串操作
    let contains_emoji = unicode_string.contains("🚀") || 
                         unicode_string.contains("🌟") || 
                         unicode_string.contains("💫") ||
                         unicode_string.contains("✨")
    
    // 不是所有字符串都包含我们测试的表情符号
    if contains_emoji {
      assert_true(unicode_string.length() >= 2)  // 表情符号 + 至少一个字符
    }
  }
}

// 测试2: 不同地区日期时间格式
test "不同地区日期时间格式测试" {
  // 定义不同地区的日期时间格式
  let locale_datetime_formats = [
    ("en-US", "MM/DD/YYYY hh:mm:ss A"),      // 美国英语
    ("en-GB", "DD/MM/YYYY HH:mm:ss"),        // 英国英语
    ("zh-CN", "YYYY年MM月DD日 HH:mm:ss"),     // 中国大陆
    ("zh-TW", "YYYY年MM月DD日 HH:mm:ss"),     // 台湾
    ("ja-JP", "YYYY/MM/DD HH:mm:ss"),        // 日本
    ("ko-KR", "YYYY. MM. DD. HH:mm:ss"),     // 韩国
    ("fr-FR", "DD/MM/YYYY HH:mm:ss"),        // 法国
    ("de-DE", "DD.MM.YYYY HH:mm:ss"),        // 德国
    ("es-ES", "DD/MM/YYYY HH:mm:ss"),        // 西班牙
    ("it-IT", "DD/MM/YYYY HH:mm:ss"),        // 意大利
    ("pt-BR", "DD/MM/YYYY HH:mm:ss"),        // 巴西葡萄牙
    ("ru-RU", "DD.MM.YYYY HH:mm:ss"),        // 俄罗斯
    ("ar-SA", "DD/MM/YYYY HH:mm:ss"),        // 沙特阿拉伯
    ("he-IL", "DD/MM/YYYY HH:mm:ss"),        // 以色列
    ("th-TH", "DD/MM/YYYY HH:mm:ss"),        // 泰国
    ("hi-IN", "DD/MM/YYYY HH:mm:ss"),        // 印度
    ("tr-TR", "DD.MM.YYYY HH:mm:ss")         // 土耳其
  ]
  
  // 测试时间戳
  let test_timestamp = 1640995200000L  // 2022-01-01 00:00:00 UTC
  
  // 验证不同地区的日期时间格式
  for (locale, format) in locale_datetime_formats {
    // 验证地区代码格式
    assert_eq(locale.length(), 5)  // xx-XX格式
    assert_eq(locale[2], '-')
    
    // 验证格式字符串包含必要的组成部分
    assert_true(format.contains("YYYY") || format.contains("YY"))
    assert_true(format.contains("MM"))
    assert_true(format.contains("DD"))
    assert_true(format.contains("HH"))
    assert_true(format.contains("mm"))
    assert_true(format.contains("ss"))
    
    // 验证格式字符串不为空
    assert_true(format.length() > 0)
    
    // 模拟日期时间格式化
    let formatted_datetime = format
      .replace("YYYY", "2022")
      .replace("MM", "01")
      .replace("DD", "01")
      .replace("HH", "00")
      .replace("mm", "00")
      .replace("ss", "00")
      .replace("A", "AM")
    
    // 验证格式化结果包含预期的日期时间
    assert_true(formatted_datetime.contains("2022") || formatted_datetime.contains("22"))
    assert_true(formatted_datetime.contains("01"))
    assert_true(formatted_datetime.contains("00"))
  }
  
  // 测试时区处理
  let timezones = [
    ("UTC", "+00:00"),
    ("America/New_York", "-05:00"),
    ("America/Los_Angeles", "-08:00"),
    ("Europe/London", "+00:00"),
    ("Europe/Paris", "+01:00"),
    ("Europe/Berlin", "+01:00"),
    ("Asia/Tokyo", "+09:00"),
    ("Asia/Shanghai", "+08:00"),
    ("Asia/Seoul", "+09:00"),
    ("Asia/Dubai", "+04:00"),
    ("Asia/Kolkata", "+05:30"),
    ("Australia/Sydney", "+11:00"),
    ("Pacific/Auckland", "+13:00")
  ]
  
  // 验证时区处理
  for (timezone_name, offset) in timezones {
    assert_true(timezone_name.length() > 0)
    assert_true(offset.length() == 6)  // ±HH:MM格式
    
    // 验证偏移量格式
    assert_true(offset[0] == '+' || offset[0] == '-')
    assert_true(offset[3] == ':')
    
    // 模拟时区转换
    let base_hour = 12
    let offset_hours = offset.substring(1, 3).to_int()
    let offset_minutes = offset.substring(4, 6).to_int()
    
    let local_hour = if offset[0] == '+' {
      (base_hour + offset_hours) % 24
    } else {
      (base_hour - offset_hours + 24) % 24
    }
    
    assert_true(local_hour >= 0 && local_hour <= 23)
  }
}

// 测试3: 数字格式和货币
test "数字格式和货币测试" {
  // 定义不同地区的数字格式
  let locale_number_formats = [
    ("en-US", "1,234,567.89"),      // 美国英语：逗号作为千位分隔符，点作为小数点
    ("en-GB", "1,234,567.89"),      // 英国英语：同美国
    ("zh-CN", "1,234,567.89"),      // 中国大陆：同美国
    ("de-DE", "1.234.567,89"),      // 德国：点作为千位分隔符，逗号作为小数点
    ("fr-FR", "1 234 567,89"),      // 法国：空格作为千位分隔符，逗号作为小数点
    ("es-ES", "1.234.567,89"),      // 西班牙：同德国
    ("it-IT", "1.234.567,89"),      // 意大利：同德国
    ("pt-BR", "1.234.567,89"),      // 巴西葡萄牙：同德国
    ("ru-RU", "1 234 567,89"),      // 俄罗斯：空格作为千位分隔符，逗号作为小数点
    ("ar-SA", "1,234,567.89"),      // 阿拉伯：同美国
    ("hi-IN", "12,34,567.89"),      // 印度：不同的千位分隔符规则
    ("tr-TR", "1.234.567,89")       // 土耳其：同德国
  ]
  
  // 验证数字格式
  for (locale, formatted_number) in locale_number_formats {
    assert_eq(locale.length(), 5)  // xx-XX格式
    assert_true(formatted_number.length() > 0)
    
    // 验证格式化数字包含数字字符
    let digit_count = 0
    for char in formatted_number.chars() {
      if char >= '0' && char <= '9' {
        digit_count = digit_count + 1
      }
    }
    
    assert_true(digit_count >= 8)  // 至少包含8个数字字符
    
    // 验证包含小数部分
    assert_true(formatted_number.contains("."))
    
    // 验证千位分隔符
    if locale == "en-US" || locale == "en-GB" || locale == "zh-CN" {
      assert_true(formatted_number.contains(","))
    } else if locale == "de-DE" || locale == "es-ES" || locale == "it-IT" || locale == "pt-BR" || locale == "tr-TR" {
      assert_true(formatted_number.contains("."))
    } else if locale == "fr-FR" || locale == "ru-RU" {
      assert_true(formatted_number.contains(" "))
    }
  }
  
  // 测试货币格式
  let currency_formats = [
    ("en-US", "USD", "$1,234.56"),      // 美元
    ("en-GB", "GBP", "£1,234.56"),      // 英镑
    ("ja-JP", "JPY", "¥1,235"),         // 日元（无小数）
    ("zh-CN", "CNY", "¥1,234.56"),      // 人民币
    ("ko-KR", "KRW", "₩1,235"),         // 韩元（无小数）
    ("de-DE", "EUR", "1.234,56 €"),     // 欧元（后置）
    ("fr-FR", "EUR", "1 234,56 €"),     // 欧元（后置，空格分隔）
    ("es-ES", "EUR", "1.234,56 €"),     // 欧元（后置）
    ("it-IT", "EUR", "€1.234,56"),      // 欧元（前置）
    ("ru-RU", "RUB", "1 234,56 ₽"),     // 卢布（后置）
    ("tr-TR", "TRY", "₺1.234,56"),      // 土耳其里拉
    ("ar-SA", "SAR", "ر.س.1,234.56"),   // 沙特里亚尔
    ("hi-IN", "INR", "₹1,234.56")       // 印度卢比
  ]
  
  // 验证货币格式
  for (locale, currency_code, formatted_currency) in currency_formats {
    assert_eq(locale.length(), 5)
    assert_eq(currency_code.length(), 3)
    assert_true(formatted_currency.length() > 0)
    
    // 验证包含货币符号
    let has_currency_symbol = 
      formatted_currency.contains("$") ||
      formatted_currency.contains("£") ||
      formatted_currency.contains("¥") ||
      formatted_currency.contains("€") ||
      formatted_currency.contains("₩") ||
      formatted_currency.contains("₽") ||
      formatted_currency.contains("₺") ||
      formatted_currency.contains("ر.س.") ||
      formatted_currency.contains("₹")
    
    assert_true(has_currency_symbol)
    
    // 验证包含数字
    let digit_count = 0
    for char in formatted_currency.chars() {
      if char >= '0' && char <= '9' {
        digit_count = digit_count + 1
      }
    }
    
    assert_true(digit_count >= 4)  // 至少包含4个数字字符
  }
}

// 测试4: 多语言错误消息
test "多语言错误消息测试" {
  // 定义多语言错误消息
  let multilingual_error_messages = [
    ("en", "Connection timeout occurred"),
    ("zh", "连接超时"),
    ("ja", "接続タイムアウトが発生しました"),
    ("ko", "연결 시간 초과가 발생했습니다"),
    ("es", "Se produjo un tiempo de espera de conexión"),
    ("fr", "Un délai de connexion s'est produit"),
    ("de", "Verbindungs-Timeout aufgetreten"),
    ("it", "Si è verificato un timeout di connessione"),
    ("pt", "Ocorreu um tempo limite de conexão"),
    ("ru", "Произошел тайм-аут подключения"),
    ("ar", "حدث انتهاء مهلة الاتصال"),
    ("hi", "कनेक्शन टाइमआउट हुआ"),
    ("tr", "Bağlantı zaman aşımı oluştu"),
    ("th", "หมดเวลาการเชื่อมต่อ"),
    ("he", "אירע זמן קצוב של חיבור")
  ]
  
  // 验证多语言错误消息
  for (language_code, error_message) in multilingual_error_messages {
    // 验证语言代码
    assert_true(language_code.length() >= 2)
    assert_true(language_code.length() <= 5)
    
    // 验证错误消息不为空
    assert_true(error_message.length() > 0)
    
    // 验证错误消息可以正确序列化和反序列化
    let serialized_message = error_message.to_string()
    assert_eq(serialized_message, error_message)
    
    // 验证错误消息包含关键词（如果适用）
    if language_code == "en" {
      assert_true(error_message.contains("timeout") || error_message.contains("Timeout"))
    } else if language_code == "zh" {
      assert_true(error_message.contains("超时"))
    } else if language_code == "ja" {
      assert_true(error_message.contains("タイムアウト"))
    }
    
    // 验证错误消息可以正确转换大小写（如果适用）
    let upper_message = error_message.to_uppercase()
    let lower_message = error_message.to_lowercase()
    
    assert_true(upper_message.length() > 0)
    assert_true(lower_message.length() > 0)
  }
  
  // 测试错误代码和多语言消息的映射
  let error_code_messages = [
    ("E001", "en", "Invalid configuration"),
    ("E001", "zh", "无效配置"),
    ("E001", "ja", "無効な構成"),
    ("E002", "en", "Resource not found"),
    ("E002", "zh", "资源未找到"),
    ("E002", "ja", "リソースが見つかりません"),
    ("E003", "en", "Access denied"),
    ("E003", "zh", "访问被拒绝"),
    ("E003", "ja", "アクセス拒否")
  ]
  
  // 验证错误代码和多语言消息映射
  for (error_code, language_code, message) in error_code_messages {
    // 验证错误代码格式
    assert_eq(error_code.length(), 4)
    assert_true(error_code[0] == 'E')
    assert_true(error_code[1] >= '0' && error_code[1] <= '9')
    assert_true(error_code[2] >= '0' && error_code[2] <= '9')
    assert_true(error_code[3] >= '0' && error_code[3] <= '9')
    
    // 验证语言代码
    assert_true(language_code.length() >= 2)
    
    // 验证消息不为空
    assert_true(message.length() > 0)
    
    // 验证消息可以正确序列化
    let serialized_message = message.to_string()
    assert_eq(serialized_message, message)
  }
}

// 测试5: 多语言属性键和值
test "多语言属性键和值测试" {
  // 定义多语言属性键和值
  let multilingual_attributes = [
    ("service.name", "服务名称", "Service Name", "サービス名", "서비스 이름"),
    ("service.version", "服务版本", "Service Version", "サービスバージョン", "서비스 버전"),
    ("host.name", "主机名", "Host Name", "ホスト名", "호스트 이름"),
    ("error.message", "错误消息", "Error Message", "エラーメッセージ", "오류 메시지"),
    ("user.id", "用户ID", "User ID", "ユーザーID", "사용자 ID"),
    ("request.id", "请求ID", "Request ID", "リクエストID", "요청 ID"),
    ("trace.id", "追踪ID", "Trace ID", "トレースID", "추적 ID"),
    ("span.name", "跨度名称", "Span Name", "スパン名", "스팬 이름"),
    ("metric.name", "度量名称", "Metric Name", "メトリック名", "메트릭 이름"),
    ("log.level", "日志级别", "Log Level", "ログレベル", "로그 레벨")
  ]
  
  // 验证多语言属性
  for (key, zh_value, en_value, ja_value, ko_value) in multilingual_attributes {
    // 验证键格式
    assert_true(key.contains("."))
    assert_true(key.length() > 0)
    
    // 验证所有语言值都不为空
    assert_true(zh_value.length() > 0)
    assert_true(en_value.length() > 0)
    assert_true(ja_value.length() > 0)
    assert_true(ko_value.length() > 0)
    
    // 验证所有语言值都可以正确序列化
    assert_eq(zh_value.to_string(), zh_value)
    assert_eq(en_value.to_string(), en_value)
    assert_eq(ja_value.to_string(), ja_value)
    assert_eq(ko_value.to_string(), ko_value)
    
    // 验证键值对可以正确组合
    let zh_pair = key + "=" + zh_value
    let en_pair = key + "=" + en_value
    let ja_pair = key + "=" + ja_value
    let ko_pair = key + "=" + ko_value
    
    assert_true(zh_pair.contains(key))
    assert_true(en_pair.contains(key))
    assert_true(ja_pair.contains(key))
    assert_true(ko_pair.contains(key))
    
    assert_true(zh_pair.contains(zh_value))
    assert_true(en_pair.contains(en_value))
    assert_true(ja_pair.contains(ja_value))
    assert_true(ko_pair.contains(ko_value))
  }
  
  // 测试多语言属性序列化和反序列化
  let attribute_sets = [
    [("service.name", "用户服务"), ("service.version", "1.0.0")],
    [("service.name", "User Service"), ("service.version", "1.0.0")],
    [("service.name", "ユーザーサービス"), ("service.version", "1.0.0")],
    [("service.name", "사용자 서비스"), ("service.version", "1.0.0")]
  ]
  
  // 验证属性集序列化
  for attributes in attribute_sets {
    let serialized = attributes.map(|(key, value)| key + "=" + value).reduce(|acc, pair| acc + "," + pair, "")
    
    // 验证序列化结果包含所有属性
    for (key, value) in attributes {
      let expected_pair = key + "=" + value
      assert_true(serialized.contains(expected_pair))
    }
    
    // 验证反序列化
    let pairs = serialized.split(",")
    assert_eq(pairs.length(), attributes.length())
    
    for pair in pairs {
      assert_true(pair.contains("="))
      let parts = pair.split("=")
      assert_eq(parts.length(), 2)
      assert_true(parts[0].length() > 0)
      assert_true(parts[1].length() > 0)
    }
  }
}

// 测试6: 地区特定的度量单位
test "地区特定的度量单位测试" {
  // 定义不同地区的度量单位
  let locale_units = [
    ("en-US", "mi", "mile", "miles"),     // 英里（美国）
    ("en-GB", "mi", "mile", "miles"),     // 英里（英国）
    ("zh-CN", "km", "kilometer", "kilometers"),  // 千米（中国）
    ("ja-JP", "km", "kilometer", "kilometers"),  // 千米（日本）
    ("ko-KR", "km", "kilometer", "kilometers"),  // 千米（韩国）
    ("de-DE", "km", "kilometer", "kilometers"),  // 千米（德国）
    ("fr-FR", "km", "kilometer", "kilometers"),  // 千米（法国）
    ("es-ES", "km", "kilometer", "kilometers"),  // 千米（西班牙）
    ("it-IT", "km", "kilometer", "kilometers"),  // 千米（意大利）
    ("pt-BR", "km", "kilometer", "kilometers"),  // 千米（巴西）
    ("ru-RU", "km", "километр", "километры"),     // 千米（俄罗斯）
    ("ar-SA", "km", "كيلومتر", "كيلومترات"),      // 千米（阿拉伯）
    ("hi-IN", "km", "किलोमीटर", "किलोमीटर"),       // 千米（印度）
    ("tr-TR", "km", "kilometre", "kilometres")     // 千米（土耳其）
  ]
  
  // 验证地区单位
  for (locale, abbreviation, singular, plural) in locale_units {
    // 验证地区代码格式
    assert_eq(locale.length(), 5)
    assert_eq(locale[2], '-')
    
    // 验证单位缩写
    assert_true(abbreviation.length() >= 1)
    assert_true(abbreviation.length() <= 3)
    
    // 验证单数形式
    assert_true(singular.length() > 0)
    
    // 验证复数形式
    assert_true(plural.length() > 0)
    
    // 验证单位可以正确序列化
    assert_eq(abbreviation.to_string(), abbreviation)
    assert_eq(singular.to_string(), singular)
    assert_eq(plural.to_string(), plural)
  }
  
  // 测试温度单位
  let temperature_units = [
    ("en-US", "F", "Fahrenheit"),      // 华氏度（美国）
    ("en-GB", "C", "Celsius"),         // 摄氏度（英国）
    ("zh-CN", "C", "摄氏度"),          // 摄氏度（中国）
    ("ja-JP", "C", "摂氏"),            // 摄氏度（日本）
    ("ko-KR", "C", "섭씨"),            // 摄氏度（韩国）
    ("de-DE", "C", "Celsius"),         // 摄氏度（德国）
    ("fr-FR", "C", "Celsius"),         // 摄氏度（法国）
    ("es-ES", "C", "Celsius"),         // 摄氏度（西班牙）
    ("ru-RU", "C", "Цельсий"),         // 摄氏度（俄罗斯）
    ("ar-SA", "C", "مئوية"),           // 摄氏度（阿拉伯）
    ("hi-IN", "C", "सेल्सियस"),        // 摄氏度（印度）
    ("tr-TR", "C", "Santigrat")        // 摄氏度（土耳其）
  ]
  
  // 验证温度单位
  for (locale, abbreviation, name) in temperature_units {
    // 验证地区代码格式
    assert_eq(locale.length(), 5)
    assert_eq(locale[2], '-')
    
    // 验证温度缩写
    assert_eq(abbreviation.length(), 1)
    assert_true(abbreviation == "F" || abbreviation == "C")
    
    // 验证温度名称
    assert_true(name.length() > 0)
    
    // 验证温度单位可以正确序列化
    assert_eq(abbreviation.to_string(), abbreviation)
    assert_eq(name.to_string(), name)
    
    // 验证温度值格式化
    let temperature_value = 25.5
    let formatted_temperature = temperature_value.to_string() + "°" + abbreviation
    
    assert_true(formatted_temperature.contains("°"))
    assert_true(formatted_temperature.contains(abbreviation))
    assert_true(formatted_temperature.contains("25.5"))
  }
  
  // 测试重量单位
  let weight_units = [
    ("en-US", "lb", "pound", "pounds"),     // 磅（美国）
    ("en-GB", "kg", "kilogram", "kilograms"),  // 千克（英国）
    ("zh-CN", "kg", "千克", "千克"),           // 千克（中国）
    ("ja-JP", "kg", "キログラム", "キログラム"),  // 千克（日本）
    ("ko-KR", "kg", "킬로그램", "킬로그램"),       // 千克（韩国）
    ("de-DE", "kg", "Kilogramm", "Kilogramm"),    // 千克（德国）
    ("fr-FR", "kg", "kilogramme", "kilogrammes"), // 千克（法国）
    ("es-ES", "kg", "kilogramo", "kilogramos"),   // 千克（西班牙）
    ("it-IT", "kg", "chilogrammo", "chilogrammi"), // 千克（意大利）
    ("pt-BR", "kg", "quilograma", "quilogramas"),  // 千克（巴西）
    ("ru-RU", "kg", "килограмм", "килограммы"),    // 千克（俄罗斯）
    ("ar-SA", "kg", "كيلوجرام", "كيلوجرامات"),     // 千克（阿拉伯）
    ("hi-IN", "kg", "किलोग्राम", "किलोग्राम"),      // 千克（印度）
    ("tr-TR", "kg", "kilogram", "kilogramlar")     // 千克（土耳其）
  ]
  
  // 验证重量单位
  for (locale, abbreviation, singular, plural) in weight_units {
    // 验证地区代码格式
    assert_eq(locale.length(), 5)
    assert_eq(locale[2], '-')
    
    // 验证重量缩写
    assert_eq(abbreviation.length(), 2)
    assert_eq(abbreviation, "kg") || assert_eq(abbreviation, "lb")
    
    // 验证单数和复数形式
    assert_true(singular.length() > 0)
    assert_true(plural.length() > 0)
    
    // 验证重量单位可以正确序列化
    assert_eq(abbreviation.to_string(), abbreviation)
    assert_eq(singular.to_string(), singular)
    assert_eq(plural.to_string(), plural)
    
    // 验证重量值格式化
    let weight_value = 75.0
    let formatted_weight_singular = weight_value.to_string() + " " + singular
    let formatted_weight_plural = (weight_value + 1.0).to_string() + " " + plural
    
    assert_true(formatted_weight_singular.contains(singular))
    assert_true(formatted_weight_plural.contains(plural))
    assert_true(formatted_weight_singular.contains("75.0"))
    assert_true(formatted_weight_plural.contains("76.0"))
  }
}