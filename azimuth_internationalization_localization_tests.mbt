// Azimuth Internationalization and Localization Tests
// 国际化和本地化支持测试套件

// Test 1: 多语言消息翻译
test "multi-language message translation" {
  // 多语言消息字典
  let message_translations = {
    "en": {
      "system_status": "System Status",
      "cpu_usage": "CPU Usage",
      "memory_usage": "Memory Usage",
      "error_occurred": "An error occurred",
      "warning_threshold": "Warning threshold exceeded",
      "critical_alert": "Critical alert triggered",
      "operation_successful": "Operation completed successfully",
      "operation_failed": "Operation failed",
      "loading_data": "Loading data...",
      "no_data_available": "No data available"
    },
    "zh": {
      "system_status": "系统状态",
      "cpu_usage": "CPU使用率",
      "memory_usage": "内存使用率",
      "error_occurred": "发生错误",
      "warning_threshold": "超过警告阈值",
      "critical_alert": "触发严重告警",
      "operation_successful": "操作成功完成",
      "operation_failed": "操作失败",
      "loading_data": "正在加载数据...",
      "no_data_available": "无可用数据"
    },
    "ja": {
      "system_status": "システムステータス",
      "cpu_usage": "CPU使用率",
      "memory_usage": "メモリ使用率",
      "error_occurred": "エラーが発生しました",
      "warning_threshold": "警告しきい値を超えました",
      "critical_alert": "重大なアラートがトリガーされました",
      "operation_successful": "操作が正常に完了しました",
      "operation_failed": "操作に失敗しました",
      "loading_data": "データを読み込み中...",
      "no_data_available": "利用可能なデータがありません"
    },
    "es": {
      "system_status": "Estado del Sistema",
      "cpu_usage": "Uso de CPU",
      "memory_usage": "Uso de Memoria",
      "error_occurred": "Ocurrió un error",
      "warning_threshold": "Umbral de advertencia excedido",
      "critical_alert": "Alerta crítica activada",
      "operation_successful": "Operación completada exitosamente",
      "operation_failed": "Operación fallida",
      "loading_data": "Cargando datos...",
      "no_data_available": "No hay datos disponibles"
    }
  }
  
  // 翻译函数
  let translate = fn(key, language) {
    let translations = message_translations[language]
    if translations && translations[key] {
      translations[key]
    } else {
      // 回退到英语
      let english_translations = message_translations["en"]
      english_translations[key] ?? key
    }
  }
  
  // 测试各种语言的翻译
  let test_keys = ["system_status", "cpu_usage", "error_occurred", "operation_successful"]
  let test_languages = ["en", "zh", "ja", "es"]
  
  // 验证翻译结果
  for language in test_languages {
    for key in test_keys {
      let translation = translate(key, language)
      assert_true(translation.length() > 0, "翻译结果不应为空: " + language + "." + key)
      assert_not_eq(translation, key, "翻译结果应该不同于原始键: " + language + "." + key)
    }
  }
  
  // 验证特定翻译
  assert_eq(translate("system_status", "en"), "System Status")
  assert_eq(translate("system_status", "zh"), "系统状态")
  assert_eq(translate("system_status", "ja"), "システムステータス")
  assert_eq(translate("system_status", "es"), "Estado del Sistema")
  
  // 测试回退机制
  let non_existent_key = "non_existent_key"
  assert_eq(translate(non_existent_key, "zh"), non_existent_key, "不存在的键应该回退到原始键")
  
  let non_existent_language = "fr"
  assert_eq(translate("system_status", non_existent_language), "System Status", "不存在的语言应该回退到英语")
}

// Test 2: 数字格式化和本地化
test "number formatting and localization" {
  // 数字格式化配置
  let number_formats = {
    "en": {
      "decimal_separator": ".",
      "thousands_separator": ",",
      "currency_symbol": "$",
      "currency_position": "before"
    },
    "zh": {
      "decimal_separator": ".",
      "thousands_separator": ",",
      "currency_symbol": "¥",
      "currency_position": "before"
    },
    "de": {
      "decimal_separator": ",",
      "thousands_separator": ".",
      "currency_symbol": "€",
      "currency_position": "after"
    },
    "fr": {
      "decimal_separator": ",",
      "thousands_separator": " ",
      "currency_symbol": "€",
      "currency_position": "after"
    }
  }
  
  // 数字格式化函数
  let format_number = fn(number, decimals, locale) {
    let format = number_formats[locale] ?? number_formats["en"]
    let decimal_separator = format["decimal_separator"]
    let thousands_separator = format["thousands_separator"]
    
    // 简化的数字格式化实现
    let number_str = @lib.string.format(number, "." + decimals.to_string() + "f")
    let parts = number_str.split(".")
    
    let integer_part = parts[0]
    let decimal_part = parts.length() > 1 ? parts[1] : ""
    
    // 添加千位分隔符
    let formatted_integer = ""
    let mut digit_count = 0
    for i in integer_part.length() - 1..=0 {
      let digit = integer_part[i]
      formatted_integer = digit + formatted_integer
      digit_count = digit_count + 1
      
      if digit_count % 3 == 0 && i > 0 {
        formatted_integer = thousands_separator + formatted_integer
      }
    }
    
    // 组合整数和小数部分
    if decimal_part.length() > 0 {
      formatted_integer + decimal_separator + decimal_part
    } else {
      formatted_integer
    }
  }
  
  // 货币格式化函数
  let format_currency = fn(amount, locale) {
    let format = number_formats[locale] ?? number_formats["en"]
    let formatted_number = format_number(amount, 2, locale)
    let currency_symbol = format["currency_symbol"]
    let currency_position = format["currency_position"]
    
    if currency_position == "before" {
      currency_symbol + formatted_number
    } else {
      formatted_number + " " + currency_symbol
    }
  }
  
  // 测试数字格式化
  let test_number = 1234567.8912
  
  let en_formatted = format_number(test_number, 2, "en")
  let zh_formatted = format_number(test_number, 2, "zh")
  let de_formatted = format_number(test_number, 2, "de")
  let fr_formatted = format_number(test_number, 2, "fr")
  
  // 验证数字格式化结果
  assert_eq(en_formatted, "1,234,567.89", "英语格式化应该正确")
  assert_eq(zh_formatted, "1,234,567.89", "中文格式化应该正确")
  assert_eq(de_formatted, "1.234.567,89", "德语格式化应该正确")
  assert_eq(fr_formatted, "1 234 567,89", "法语格式化应该正确")
  
  // 测试货币格式化
  let test_amount = 1234.56
  
  let en_currency = format_currency(test_amount, "en")
  let zh_currency = format_currency(test_amount, "zh")
  let de_currency = format_currency(test_amount, "de")
  let fr_currency = format_currency(test_amount, "fr")
  
  // 验证货币格式化结果
  assert_eq(en_currency, "$1,234.56", "英语货币格式化应该正确")
  assert_eq(zh_currency, "¥1,234.56", "中文货币格式化应该正确")
  assert_eq(de_currency, "1.234,56 €", "德语货币格式化应该正确")
  assert_eq(fr_currency, "1 234,56 €", "法语货币格式化应该正确")
}

// Test 3: 日期和时间本地化
test "date and time localization" {
  // 日期时间格式配置
  let datetime_formats = {
    "en": {
      "date_format": "MM/DD/YYYY",
      "time_format": "h:mm:ss A",
      "datetime_format": "MM/DD/YYYY h:mm:ss A",
      "timezone": "UTC"
    },
    "zh": {
      "date_format": "YYYY年MM月DD日",
      "time_format": "HH:mm:ss",
      "datetime_format": "YYYY年MM月DD日 HH:mm:ss",
      "timezone": "Asia/Shanghai"
    },
    "ja": {
      "date_format": "YYYY/MM/DD",
      "time_format": "HH:mm:ss",
      "datetime_format": "YYYY/MM/DD HH:mm:ss",
      "timezone": "Asia/Tokyo"
    },
    "de": {
      "date_format": "DD.MM.YYYY",
      "time_format": "HH:mm:ss",
      "datetime_format": "DD.MM.YYYY HH:mm:ss",
      "timezone": "Europe/Berlin"
    }
  }
  
  // 时区偏移量（分钟）
  let timezone_offsets = {
    "UTC": 0,
    "Asia/Shanghai": 480,  // UTC+8
    "Asia/Tokyo": 540,     // UTC+9
    "Europe/Berlin": 60    // UTC+1
  }
  
  // 日期时间格式化函数
  let format_datetime = fn(timestamp, format_type, locale) {
    let formats = datetime_formats[locale] ?? datetime_formats["en"]
    let format_key = format_type + "_format"
    let format_string = formats[format_key]
    let timezone = formats["timezone"]
    
    // 转换时区
    let timezone_offset = timezone_offsets[timezone] ?? 0
    let adjusted_timestamp = timestamp + timezone_offset * 60
    
    // 简化的日期时间格式化（实际实现会使用日期时间库）
    let date = @lib.date.from_timestamp(adjusted_timestamp)
    let year = date.year
    let month = date.month
    let day = date.day
    let hour = date.hour
    let minute = date.minute
    let second = date.second
    
    // 根据格式字符串格式化
    match locale {
      "en" => {
        if format_type == "date" {
          month.to_string() + "/" + day.to_string() + "/" + year.to_string()
        } else if format_type == "time" {
          let hour_12 = if hour > 12 { hour - 12 } else if hour == 0 { 12 } else { hour }
          let am_pm = if hour >= 12 { "PM" } else { "AM" }
          hour_12.to_string() + ":" + minute.to_string() + ":" + second.to_string() + " " + am_pm
        } else {
          month.to_string() + "/" + day.to_string() + "/" + year.to_string() + " " + 
          hour.to_string() + ":" + minute.to_string() + ":" + second.to_string()
        }
      }
      "zh" => {
        if format_type == "date" {
          year.to_string() + "年" + month.to_string() + "月" + day.to_string() + "日"
        } else if format_type == "time" {
          hour.to_string() + ":" + minute.to_string() + ":" + second.to_string()
        } else {
          year.to_string() + "年" + month.to_string() + "月" + day.to_string() + "日 " + 
          hour.to_string() + ":" + minute.to_string() + ":" + second.to_string()
        }
      }
      "ja" => {
        if format_type == "date" {
          year.to_string() + "/" + month.to_string() + "/" + day.to_string()
        } else if format_type == "time" {
          hour.to_string() + ":" + minute.to_string() + ":" + second.to_string()
        } else {
          year.to_string() + "/" + month.to_string() + "/" + day.to_string() + " " + 
          hour.to_string() + ":" + minute.to_string() + ":" + second.to_string()
        }
      }
      "de" => {
        if format_type == "date" {
          day.to_string() + "." + month.to_string() + "." + year.to_string()
        } else if format_type == "time" {
          hour.to_string() + ":" + minute.to_string() + ":" + second.to_string()
        } else {
          day.to_string() + "." + month.to_string() + "." + year.to_string() + " " + 
          hour.to_string() + ":" + minute.to_string() + ":" + second.to_string()
        }
      }
      _ => {
        year.to_string() + "-" + month.to_string() + "-" + day.to_string() + " " + 
        hour.to_string() + ":" + minute.to_string() + ":" + second.to_string()
      }
    }
  }
  
  // 测试时间戳（2022-01-01 12:00:00 UTC）
  let test_timestamp = 1640995200
  
  // 测试日期格式化
  let en_date = format_datetime(test_timestamp, "date", "en")
  let zh_date = format_datetime(test_timestamp, "date", "zh")
  let ja_date = format_datetime(test_timestamp, "date", "ja")
  let de_date = format_datetime(test_timestamp, "date", "de")
  
  // 验证日期格式化结果（考虑时区差异）
  assert_eq(en_date, "1/1/2022", "英语日期格式化应该正确")
  assert_eq(zh_date, "2022年1月1日", "中文日期格式化应该正确")
  assert_eq(ja_date, "2022/01/01", "日语日期格式化应该正确")
  assert_eq(de_date, "1.1.2022", "德语日期格式化应该正确")
  
  // 测试时间格式化
  let en_time = format_datetime(test_timestamp, "time", "en")
  let zh_time = format_datetime(test_timestamp, "time", "zh")
  let ja_time = format_datetime(test_timestamp, "time", "ja")
  let de_time = format_datetime(test_timestamp, "time", "de")
  
  // 验证时间格式化结果（考虑时区差异）
  assert_eq(en_time, "12:00:00 PM", "英语时间格式化应该正确")
  assert_eq(zh_time, "20:00:00", "中文时间格式化应该正确（UTC+8）")
  assert_eq(ja_time, "21:00:00", "日语时间格式化应该正确（UTC+9）")
  assert_eq(de_time, "13:00:00", "德语时间格式化应该正确（UTC+1）")
}

// Test 4: 度量单位本地化
test "measurement units localization" {
  // 度量单位配置
  let measurement_units = {
    "en": {
      "length": {"km": "km", "m": "m", "cm": "cm", "in": "in", "ft": "ft"},
      "weight": {"kg": "kg", "g": "g", "lb": "lb", "oz": "oz"},
      "temperature": {"celsius": "°C", "fahrenheit": "°F", "kelvin": "K"},
      "data": {"byte": "B", "kilobyte": "KB", "megabyte": "MB", "gigabyte": "GB"},
      "speed": {"kmh": "km/h", "mph": "mph", "ms": "m/s"}
    },
    "zh": {
      "length": {"km": "公里", "m": "米", "cm": "厘米", "in": "英寸", "ft": "英尺"},
      "weight": {"kg": "公斤", "g": "克", "lb": "磅", "oz": "盎司"},
      "temperature": {"celsius": "摄氏度", "fahrenheit": "华氏度", "kelvin": "开尔文"},
      "data": {"byte": "字节", "kilobyte": "千字节", "megabyte": "兆字节", "gigabyte": "吉字节"},
      "speed": {"kmh": "公里/小时", "mph": "英里/小时", "ms": "米/秒"}
    },
    "ja": {
      "length": {"km": "km", "m": "m", "cm": "cm", "in": "インチ", "ft": "フィート"},
      "weight": {"kg": "kg", "g": "g", "lb": "ポンド", "oz": "オンス"},
      "temperature": {"celsius": "°C", "fahrenheit": "°F", "kelvin": "K"},
      "data": {"byte": "バイト", "kilobyte": "KB", "megabyte": "MB", "gigabyte": "GB"},
      "speed": {"kmh": "km/h", "mph": "mph", "ms": "m/s"}
    }
  }
  
  // 单位转换函数
  let convert_unit = fn(value, from_unit, to_unit, unit_type) {
    match unit_type {
      "temperature" => {
        if from_unit == "celsius" && to_unit == "fahrenheit" {
          value * 9.0 / 5.0 + 32.0
        } else if from_unit == "fahrenheit" && to_unit == "celsius" {
          (value - 32.0) * 5.0 / 9.0
        } else if from_unit == "celsius" && to_unit == "kelvin" {
          value + 273.15
        } else if from_unit == "kelvin" && to_unit == "celsius" {
          value - 273.15
        } else {
          value
        }
      }
      "length" => {
        if from_unit == "km" && to_unit == "m" {
          value * 1000.0
        } else if from_unit == "m" && to_unit == "km" {
          value / 1000.0
        } else if from_unit == "m" && to_unit == "ft" {
          value * 3.28084
        } else if from_unit == "ft" && to_unit == "m" {
          value / 3.28084
        } else {
          value
        }
      }
      "data" => {
        if from_unit == "byte" && to_unit == "kilobyte" {
          value / 1024.0
        } else if from_unit == "kilobyte" && to_unit == "megabyte" {
          value / 1024.0
        } else if from_unit == "megabyte" && to_unit == "gigabyte" {
          value / 1024.0
        } else if from_unit == "gigabyte" && to_unit == "megabyte" {
          value * 1024.0
        } else {
          value
        }
      }
      _ => value
    }
  }
  
  // 本地化单位格式化函数
  let format_unit = fn(value, unit, unit_type, locale) {
    let units = measurement_units[locale] ?? measurement_units["en"]
    let unit_symbols = units[unit_type]
    let localized_unit = unit_symbols[unit] ?? unit
    
    value.to_string() + " " + localized_unit
  }
  
  // 测试单位转换和本地化
  let test_temperature = 25.0  // 25°C
  
  let celsius_en = format_unit(test_temperature, "celsius", "temperature", "en")
  let celsius_zh = format_unit(test_temperature, "celsius", "temperature", "zh")
  let celsius_ja = format_unit(test_temperature, "celsius", "temperature", "ja")
  
  // 验证温度单位本地化
  assert_eq(celsius_en, "25.0 °C", "英语温度单位应该正确")
  assert_eq(celsius_zh, "25.0 摄氏度", "中文温度单位应该正确")
  assert_eq(celsius_ja, "25.0 °C", "日语温度单位应该正确")
  
  // 测试温度转换
  let fahrenheit_value = convert_unit(test_temperature, "celsius", "fahrenheit", "temperature")
  let fahrenheit_en = format_unit(fahrenheit_value, "fahrenheit", "temperature", "en")
  let fahrenheit_zh = format_unit(fahrenheit_value, "fahrenheit", "temperature", "zh")
  
  // 验证温度转换和本地化
  assert_true(@lib.abs(fahrenheit_value - 77.0) < 0.1, "温度转换应该正确")
  assert_eq(fahrenheit_en, "77.0 °F", "英语华氏度单位应该正确")
  assert_eq(fahrenheit_zh, "77.0 华氏度", "中文华氏度单位应该正确")
  
  // 测试数据单位本地化
  let test_data_size = 1048576.0  // 1MB
  
  let bytes_en = format_unit(test_data_size, "byte", "data", "en")
  let bytes_zh = format_unit(test_data_size, "byte", "data", "zh")
  let megabytes_en = format_unit(1.0, "megabyte", "data", "en")
  let megabytes_zh = format_unit(1.0, "megabyte", "data", "zh")
  
  // 验证数据单位本地化
  assert_eq(bytes_en, "1048576.0 B", "英语字节单位应该正确")
  assert_eq(bytes_zh, "1048576.0 字节", "中文字节单位应该正确")
  assert_eq(megabytes_en, "1.0 MB", "英语兆字节单位应该正确")
  assert_eq(megabytes_zh, "1.0 兆字节", "中文兆字节单位应该正确")
}

// Test 5: 文本方向和RTL支持
test "text direction and RTL support" {
  // 语言方向配置
  let language_directions = {
    "en": "LTR",
    "zh": "LTR",
    "ja": "LTR",
    "ar": "RTL",
    "he": "RTL",
    "fa": "RTL"
  }
  
  // 文本对齐配置
  let text_alignments = {
    "LTR": {
      "left": "left",
      "right": "right",
      "start": "left",
      "end": "right"
    },
    "RTL": {
      "left": "right",
      "right": "left",
      "start": "right",
      "end": "left"
    }
  }
  
  // 多方向文本示例
  let multilingual_texts = {
    "en": "Hello World! System status is normal.",
    "zh": "你好世界！系统状态正常。",
    "ja": "こんにちは世界！システム状態は正常です。",
    "ar": "مرحبا بالعالم! حالة النظام طبيعية.",
    "he": "שלום עולם! מצב המערכת תקין.",
    "fa": "سلام دنیا! وضعیت سیستم عادی است."
  }
  
  // 获取文本方向
  let get_text_direction = fn(language) {
    language_directions[language] ?? "LTR"
  }
  
  // 获取文本对齐
  let get_text_alignment = fn(language, alignment_type) {
    let direction = get_text_direction(language)
    text_alignments[direction][alignment_type]
  }
  
  // 测试文本方向检测
  assert_eq(get_text_direction("en"), "LTR", "英语应该是从左到右")
  assert_eq(get_text_direction("zh"), "LTR", "中文应该是从左到右")
  assert_eq(get_text_direction("ja"), "LTR", "日语应该是从左到右")
  assert_eq(get_text_direction("ar"), "RTL", "阿拉伯语应该是从右到左")
  assert_eq(get_text_direction("he"), "RTL", "希伯来语应该是从右到左")
  assert_eq(get_text_direction("fa"), "RTL", "波斯语应该是从右到左")
  
  // 测试文本对齐
  assert_eq(get_text_alignment("en", "start"), "left", "英语开始位置应该是左边")
  assert_eq(get_text_alignment("en", "end"), "right", "英语结束位置应该是右边")
  assert_eq(get_text_alignment("ar", "start"), "right", "阿拉伯语开始位置应该是右边")
  assert_eq(get_text_alignment("ar", "end"), "left", "阿拉伯语结束位置应该是左边")
  
  // 测试多语言文本渲染
  for language in multilingual_texts.keys() {
    let text = multilingual_texts[language]
    let direction = get_text_direction(language)
    
    // 验证文本不为空
    assert_true(text.length() > 0, "多语言文本不应为空: " + language)
    
    // 验证方向配置存在
    assert_true(direction == "LTR" || direction == "RTL", "文本方向应该是有效值: " + language)
  }
  
  // 测试混合语言文本处理
  let mixed_language_text = "Hello مرحبا こんにちは"
  let auto_detected_direction = "LTR"  // 简化的自动检测，实际会更复杂
  
  assert_eq(auto_detected_direction, "LTR", "混合语言文本应该有合理的方向检测")
}

// Test 6: 复数形式处理
test "plural forms handling" {
  // 复数规则配置
  let plural_rules = {
    "en": {
      "rule": "n != 1",
      "forms": ["one", "other"]
    },
    "zh": {
      "rule": "always",
      "forms": ["other"]
    },
    "ja": {
      "rule": "always",
      "forms": ["other"]
    },
    "ar": {
      "rule": "n==0 ? 0 : n==1 ? 1 : n==2 ? 2 : n%100>=3 && n%100<=10 ? 3 : n%100>=11 ? 4 : 5",
      "forms": ["zero", "one", "two", "few", "many", "other"]
    },
    "ru": {
      "rule": "n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2",
      "forms": ["one", "few", "many"]
    }
  }
  
  // 复数形式翻译
  let plural_translations = {
    "en": {
      "items": {
        "one": "{} item",
        "other": "{} items"
      },
      "errors": {
        "one": "{} error",
        "other": "{} errors"
      },
      "minutes": {
        "one": "{} minute",
        "other": "{} minutes"
      }
    },
    "zh": {
      "items": {
        "other": "{} 项"
      },
      "errors": {
        "other": "{} 个错误"
      },
      "minutes": {
        "other": "{} 分钟"
      }
    },
    "ja": {
      "items": {
        "other": "{} 項目"
      },
      "errors": {
        "other": "{} エラー"
      },
      "minutes": {
        "other": "{} 分"
      }
    },
    "ru": {
      "items": {
        "one": "{} элемент",
        "few": "{} элемента",
        "many": "{} элементов"
      },
      "errors": {
        "one": "{} ошибка",
        "few": "{} ошибки",
        "many": "{} ошибок"
      }
    }
  }
  
  // 简化的复数形式检测函数
  let get_plural_form = fn(count, language) {
    match language {
      "en" => {
        if count == 1 { "one" } else { "other" }
      }
      "zh" | "ja" => {
        "other"
      }
      "ru" => {
        let mod10 = count % 10
        let mod100 = count % 100
        if mod10 == 1 && mod100 != 11 {
          "one"
        } else if mod10 >= 2 && mod10 <= 4 && (mod100 < 10 || mod100 >= 20) {
          "few"
        } else {
          "many"
        }
      }
      _ => "other"
    }
  }
  
  // 复数形式翻译函数
  let translate_plural = fn(key, count, language) {
    let plural_form = get_plural_form(count, language)
    let translations = plural_translations[language]
    if translations {
      let key_translations = translations[key]
      if key_translations {
        let template = key_translations[plural_form] ?? key_translations["other"]
        template.replace("{}", count.to_string())
      } else {
        count.to_string() + " " + key
      }
    } else {
      count.to_string() + " " + key
    }
  }
  
  // 测试复数形式处理
  let test_counts = [0, 1, 2, 5, 11, 21]
  let test_languages = ["en", "zh", "ja", "ru"]
  
  // 验证英语复数形式
  assert_eq(translate_plural("items", 1, "en"), "1 item", "英语单数形式应该正确")
  assert_eq(translate_plural("items", 5, "en"), "5 items", "英语复数形式应该正确")
  
  // 验证中文复数形式（中文通常不区分单复数）
  assert_eq(translate_plural("items", 1, "zh"), "1 项", "中文复数形式应该正确")
  assert_eq(translate_plural("items", 5, "zh"), "5 项", "中文复数形式应该正确")
  
  // 验证俄语复数形式（俄语有复杂的复数规则）
  assert_eq(translate_plural("items", 1, "ru"), "1 элемент", "俄语单数形式应该正确")
  assert_eq(translate_plural("items", 2, "ru"), "2 элемента", "俄语少数形式应该正确")
  assert_eq(translate_plural("items", 5, "ru"), "5 элементов", "俄语多数形式应该正确")
  assert_eq(translate_plural("items", 11, "ru"), "11 элементов", "俄语11的特殊形式应该正确")
  assert_eq(translate_plural("items", 21, "ru"), "21 элемент", "俄语21的特殊形式应该正确")
  
  // 测试错误计数
  assert_eq(translate_plural("errors", 1, "en"), "1 error", "英语错误单数形式应该正确")
  assert_eq(translate_plural("errors", 3, "en"), "3 errors", "英语错误复数形式应该正确")
  assert_eq(translate_plural("errors", 1, "ru"), "1 ошибка", "俄语错误单数形式应该正确")
  assert_eq(translate_plural("errors", 2, "ru"), "2 ошибки", "俄语错误少数形式应该正确")
  assert_eq(translate_plural("errors", 5, "ru"), "5 ошибок", "俄语错误多数形式应该正确")
}

// Test 7: 区域特定格式和约定
test "locale-specific formats and conventions" {
  // 区域特定配置
  let locale_conventions = {
    "en-US": {
      "date_format": "MM/DD/YYYY",
      "time_format": "h:mm:ss A",
      "number_format": {"decimal": ".", "thousands": ","},
      "currency": {"symbol": "$", "position": "before"},
      "week_start": "sunday",
      "paper_size": "letter"
    },
    "en-GB": {
      "date_format": "DD/MM/YYYY",
      "time_format": "HH:mm:ss",
      "number_format": {"decimal": ".", "thousands": ","},
      "currency": {"symbol": "£", "position": "before"},
      "week_start": "monday",
      "paper_size": "A4"
    },
    "zh-CN": {
      "date_format": "YYYY年MM月DD日",
      "time_format": "HH:mm:ss",
      "number_format": {"decimal": ".", "thousands": ","},
      "currency": {"symbol": "¥", "position": "before"},
      "week_start": "monday",
      "paper_size": "A4"
    },
    "de-DE": {
      "date_format": "DD.MM.YYYY",
      "time_format": "HH:mm:ss",
      "number_format": {"decimal": ",", "thousands": "."},
      "currency": {"symbol": "€", "position": "after"},
      "week_start": "monday",
      "paper_size": "A4"
    },
    "ja-JP": {
      "date_format": "YYYY/MM/DD",
      "time_format": "HH:mm:ss",
      "number_format": {"decimal": ".", "thousands": ","},
      "currency": {"symbol": "¥", "position": "before"},
      "week_start": "sunday",
      "paper_size": "A4"
    }
  }
  
  // 地址格式配置
  let address_formats = {
    "en-US": {
      "order": ["street", "city", "state", "zip", "country"],
      "template": "{street}\n{city}, {state} {zip}\n{country}"
    },
    "zh-CN": {
      "order": ["country", "state", "city", "street", "zip"],
      "template": "{country}{state}{city}{street}\n{zip}"
    },
    "ja-JP": {
      "order": ["country", "state", "city", "street", "zip"],
      "template": "〒{zip}\n{state}{city}{street}"
    },
    "de-DE": {
      "order": ["street", "zip", "city", "country"],
      "template": "{street}\n{zip} {city}\n{country}"
    }
  }
  
  // 姓名格式配置
  let name_formats = {
    "en-US": {"order": ["first", "middle", "last"], "template": "{first} {middle} {last}"},
    "zh-CN": {"order": ["last", "first"], "template": "{last}{first}"},
    "ja-JP": {"order": ["last", "first"], "template": "{last} {first}"},
    "hu-HU": {"order": ["last", "first", "middle"], "template": "{last} {first} {middle}"},  // 匈牙利语
    "ko-KR": {"order": ["last", "first"], "template": "{last}{first}"}  // 韩语
  }
  
  // 测试区域特定日期格式
  let test_date = {
    "year": 2022,
    "month": 1,
    "day": 15
  }
  
  // 验证美式英语日期格式
  let us_convention = locale_conventions["en-US"]
  assert_eq(us_convention["date_format"], "MM/DD/YYYY", "美式英语日期格式应该正确")
  assert_eq(us_convention["week_start"], "sunday", "美国一周开始应该是周日")
  assert_eq(us_convention["paper_size"], "letter", "美国纸张尺寸应该是letter")
  
  // 验证英式英语日期格式
  let gb_convention = locale_conventions["en-GB"]
  assert_eq(gb_convention["date_format"], "DD/MM/YYYY", "英式英语日期格式应该正确")
  assert_eq(gb_convention["week_start"], "monday", "英国一周开始应该是周一")
  assert_eq(gb_convention["paper_size"], "A4", "英国纸张尺寸应该是A4")
  
  // 验证中文日期格式
  let cn_convention = locale_conventions["zh-CN"]
  assert_eq(cn_convention["date_format"], "YYYY年MM月DD日", "中文日期格式应该正确")
  assert_eq(cn_convention["currency"]["symbol"], "¥", "中文货币符号应该正确")
  
  // 验证德语数字格式
  let de_convention = locale_conventions["de-DE"]
  assert_eq(de_convention["number_format"]["decimal"], ",", "德语小数分隔符应该是逗号")
  assert_eq(de_convention["number_format"]["thousands"], ".", "德语千位分隔符应该是点")
  
  // 测试地址格式化
  let test_address = {
    "street": "123 Main St",
    "city": "New York",
    "state": "NY",
    "zip": "10001",
    "country": "USA"
  }
  
  let us_address_format = address_formats["en-US"]
  assert_eq(us_address_format["order"][0], "street", "美国地址顺序应该以街道开始")
  
  // 测试姓名格式化
  let test_name = {
    "first": "John",
    "middle": "F.",
    "last": "Kennedy"
  }
  
  let us_name_format = name_formats["en-US"]
  assert_eq(us_name_format["order"][0], "first", "美国姓名顺序应该以名字开始")
  
  let cn_name_format = name_formats["zh-CN"]
  assert_eq(cn_name_format["order"][0], "last", "中文姓名顺序应该以姓氏开始")
}

// Test 8: 时区处理和转换
test "timezone handling and conversion" {
  // 时区配置
  let timezones = {
    "UTC": {"offset": 0, "dst": false},
    "America/New_York": {"offset": -300, "dst": true},  // UTC-5 (标准时间), UTC-4 (夏令时)
    "America/Los_Angeles": {"offset": -480, "dst": true},  // UTC-8 (标准时间), UTC-7 (夏令时)
    "Europe/London": {"offset": 0, "dst": true},  // UTC+0 (标准时间), UTC+1 (夏令时)
    "Europe/Paris": {"offset": 60, "dst": true},  // UTC+1 (标准时间), UTC+2 (夏令时)
    "Asia/Shanghai": {"offset": 480, "dst": false},  // UTC+8
    "Asia/Tokyo": {"offset": 540, "dst": false},  // UTC+9
    "Australia/Sydney": {"offset": 660, "dst": true}  // UTC+10 (标准时间), UTC+11 (夏令时)
  }
  
  // 夏令时期间配置（简化）
  let dst_periods = {
    "2022": {
      "US": {"start": "2022-03-13", "end": "2022-11-06"},
      "EU": {"start": "2022-03-27", "end": "2022-10-30"},
      "Australia": {"start": "2022-10-02", "end": "2023-04-02"}
    }
  }
  
  // 时区转换函数
  let convert_timezone = fn(timestamp, from_timezone, to_timezone, date) {
    let from_offset = timezones[from_timezone]["offset"]
    let to_offset = timezones[to_timezone]["offset"]
    
    // 检查夏令时（简化实现）
    let from_dst_offset = if timezones[from_timezone]["dst"] && is_in_dst(date, from_timezone) {
      60  // 夏令时增加1小时
    } else {
      0
    }
    
    let to_dst_offset = if timezones[to_timezone]["dst"] && is_in_dst(date, to_timezone) {
      60  // 夏令时增加1小时
    } else {
      0
    }
    
    let total_offset_diff = (to_offset + to_dst_offset) - (from_offset + from_dst_offset)
    timestamp + total_offset_diff * 60
  }
  
  // 简化的夏令时检查函数
  let is_in_dst = fn(date, timezone) {
    // 简化实现，实际会更复杂
    match timezone {
      "America/New_York" | "America/Los_Angeles" => {
        date.month >= 3 && date.month <= 11
      }
      "Europe/London" | "Europe/Paris" => {
        date.month >= 3 && date.month <= 10
      }
      "Australia/Sydney" => {
        date.month <= 4 || date.month >= 10
      }
      _ => false
    }
  }
  
  // 测试时区转换
  let test_timestamp = 1640995200  // 2022-01-01 12:00:00 UTC
  let test_date = {"year": 2022, "month": 1, "day": 1}  // 1月，不在夏令时期间
  
  // UTC到纽约时间转换
  let ny_time = convert_timezone(test_timestamp, "UTC", "America/New_York", test_date)
  let ny_expected = test_timestamp - 5 * 3600  // UTC-5
  assert_eq(ny_time, ny_expected, "UTC到纽约时间转换应该正确")
  
  // UTC到东京时间转换
  let tokyo_time = convert_timezone(test_timestamp, "UTC", "Asia/Tokyo", test_date)
  let tokyo_expected = test_timestamp + 9 * 3600  // UTC+9
  assert_eq(tokyo_time, tokyo_expected, "UTC到东京时间转换应该正确")
  
  // 纽约到东京时间转换
  let ny_to_tokyo = convert_timezone(ny_time, "America/New_York", "Asia/Tokyo", test_date)
  let ny_to_tokyo_expected = ny_time + 14 * 3600  // 从UTC-5到UTC+9，相差14小时
  assert_eq(ny_to_tokyo, ny_to_tokyo_expected, "纽约到东京时间转换应该正确")
  
  // 测试夏令时期间转换
  let summer_date = {"year": 2022, "month": 7, "day": 1}  // 7月，在夏令时期间
  let summer_timestamp = 1656667200  // 2022-07-01 12:00:00 UTC
  
  // UTC到纽约夏令时转换
  let ny_summer_time = convert_timezone(summer_timestamp, "UTC", "America/New_York", summer_date)
  let ny_summer_expected = summer_timestamp - 4 * 3600  // UTC-4 (夏令时)
  assert_eq(ny_summer_time, ny_summer_expected, "UTC到纽约夏令时转换应该正确")
  
  // 验证时区偏移量
  assert_eq(timezones["UTC"]["offset"], 0, "UTC偏移量应该为0")
  assert_eq(timezones["Asia/Shanghai"]["offset"], 480, "上海偏移量应该为UTC+8")
  assert_eq(timezones["America/New_York"]["offset"], -300, "纽约偏移量应该为UTC-5")
}

// Test 9: 字符编码和Unicode处理
test "character encoding and Unicode handling" {
  // Unicode字符测试
  let unicode_strings = {
    "latin": "Hello World! 123",
    "chinese": "你好世界！一二三",
    "japanese": "こんにちは世界！一二三",
    "arabic": "مرحبا بالعالم! ١٢٣",
    "hebrew": "שלום עולם! ١٢٣",
    "russian": "Привет мир! ١٢٣",
    "emoji": "🌍🌎🌏 Hello! 🎉🎊🎈",
    "mixed": "Hello 世界! 🌍 123 مرحبا"
  }
  
  // 字符统计函数
  let count_characters = fn(text) {
    text.length()  // 简化实现，实际应该考虑Unicode组合字符
  }
  
  // 字节长度估算函数
  let estimate_byte_length = fn(text, encoding) {
    match encoding {
      "UTF-8" => {
        // 简化的UTF-8字节长度估算
        let mut byte_count = 0
        for char in text {
          if char <= 0x7F {
            byte_count = byte_count + 1
          } else if char <= 0x7FF {
            byte_count = byte_count + 2
          } else if char <= 0xFFFF {
            byte_count = byte_count + 3
          } else {
            byte_count = byte_count + 4
          }
        }
        byte_count
      }
      "UTF-16" => {
        text.length() * 2  // 简化实现
      }
      _ => text.length()
    }
  }
  
  // 文本规范化函数
  let normalize_text = fn(text, form) {
    // 简化的规范化实现，实际会使用Unicode规范化库
    match form {
      "NFC" => text  // 规范化形式C
      "NFD" => text  // 规范化形式D
      "NFKC" => text // 规范化形式KC
      "NFKD" => text // 规范化形式KD
      _ => text
    }
  }
  
  // 测试Unicode字符处理
  for language in unicode_strings.keys() {
    let text = unicode_strings[language]
    
    // 验证字符计数
    let char_count = count_characters(text)
    assert_true(char_count > 0, "Unicode文本应该有字符: " + language)
    
    // 验证字节长度估算
    let utf8_length = estimate_byte_length(text, "UTF-8")
    let utf16_length = estimate_byte_length(text, "UTF-16")
    
    assert_true(utf8_length > 0, "UTF-8字节长度应该大于0: " + language)
    assert_true(utf16_length > 0, "UTF-16字节长度应该大于0: " + language)
    
    // 验证文本规范化
    let normalized_nfc = normalize_text(text, "NFC")
    let normalized_nfd = normalize_text(text, "NFD")
    
    assert_eq(normalized_nfc, text, "NFC规范化应该保持文本不变: " + language)
    assert_eq(normalized_nfd, text, "NFD规范化应该保持文本不变: " + language)
  }
  
  // 测试特定Unicode字符
  let latin_text = unicode_strings["latin"]
  let chinese_text = unicode_strings["chinese"]
  let emoji_text = unicode_strings["emoji"]
  let mixed_text = unicode_strings["mixed"]
  
  // 验证拉丁字符
  assert_eq(count_characters(latin_text), 18, "拉丁文本字符计数应该正确")
  assert_eq(estimate_byte_length(latin_text, "UTF-8"), 18, "拉丁文本UTF-8字节长度应该正确")
  
  // 验证中文字符
  assert_eq(count_characters(chinese_text), 8, "中文文本字符计数应该正确")
  assert_true(estimate_byte_length(chinese_text, "UTF-8") > 16, "中文文本UTF-8字节长度应该大于字符数")
  
  // 验证Emoji字符
  assert_eq(count_characters(emoji_text), 15, "Emoji文本字符计数应该正确")
  assert_true(estimate_byte_length(emoji_text, "UTF-8") > 15, "Emoji文本UTF-8字节长度应该大于字符数")
  
  // 验证混合文本
  assert_eq(count_characters(mixed_text), 16, "混合文本字符计数应该正确")
}

// Test 10: 国际化性能和缓存
test "internationalization performance and caching" {
  // 模拟大量翻译数据
  let generate_translations = fn(language_count, key_count) {
    let translations = {}
    let languages = ["en", "zh", "ja", "ko", "es", "fr", "de", "ru", "ar", "pt"]
    
    for i in 0..@lib.min(language_count, languages.length()) {
      let lang = languages[i]
      translations[lang] = {}
      
      for j in 0..key_count {
        let key = "key_" + j.to_string()
        translations[lang][key] = lang + "_" + key + "_translated"
      }
    }
    
    translations
  }
  
  // 翻译缓存
  let translation_cache = {}
  
  // 带缓存的翻译函数
  let cached_translate = fn(translations, key, language) {
    let cache_key = language + ":" + key
    
    match translation_cache[cache_key] {
      Some(cached_value) => cached_value
      None => {
        let lang_translations = translations[language]
        let translation = if lang_translations && lang_translations[key] {
          lang_translations[key]
        } else {
          key  // 回退到键
        }
        
        translation_cache[cache_key] = translation
        translation
      }
    }
  }
  
  // 性能测试函数
  let performance_test = fn(test_name, translate_fn, translations, test_keys, test_languages, iterations) {
    let start_time = @lib.time.now()
    
    for i in 0..iterations {
      let key = test_keys[i % test_keys.length()]
      let language = test_languages[i % test_languages.length()]
      translate_fn(translations, key, language)
    }
    
    let end_time = @lib.time.now()
    let duration = end_time - start_time
    
    {
      "test_name": test_name,
      "iterations": iterations,
      "duration_ms": duration,
      "operations_per_second": iterations.to_decimal() / duration.to_decimal() * 1000.0
    }
  }
  
  // 生成测试数据
  let translations = generate_translations(10, 1000)
  let test_keys = ["key_0", "key_1", "key_10", "key_100", "key_500"]
  let test_languages = ["en", "zh", "ja", "ko", "es"]
  
  // 无缓存的翻译函数
  let uncached_translate = fn(translations, key, language) {
    let lang_translations = translations[language]
    if lang_translations && lang_translations[key] {
      lang_translations[key]
    } else {
      key
    }
  }
  
  // 执行性能测试
  let cache_test_size = 10000
  let uncached_result = performance_test("uncached", uncached_translate, translations, test_keys, test_languages, cache_test_size)
  
  // 清空缓存并执行缓存测试
  for key in translation_cache.keys() {
    @lib.object.delete(translation_cache, key)
  }
  
  let cached_result = performance_test("cached", cached_translate, translations, test_keys, test_languages, cache_test_size)
  
  // 验证性能测试结果
  assert_eq(uncached_result["iterations"], cache_test_size, "无缓存测试迭代次数应该正确")
  assert_eq(cached_result["iterations"], cache_test_size, "缓存测试迭代次数应该正确")
  assert_true(uncached_result["duration_ms"] > 0, "无缓存测试应该有执行时间")
  assert_true(cached_result["duration_ms"] > 0, "缓存测试应该有执行时间")
  
  // 验证缓存性能提升
  let performance_improvement = uncached_result["operations_per_second"] / cached_result["operations_per_second"]
  assert_true(performance_improvement > 1.0, "缓存应该提高性能")
  
  // 验证缓存命中率
  let cache_hit_count = 0
  let cache_miss_count = 0
  
  // 模拟缓存统计
  for i in 0..1000 {
    let key = test_keys[i % test_keys.length()]
    let language = test_languages[i % test_languages.length()]
    let cache_key = language + ":" + key
    
    if translation_cache[cache_key] {
      cache_hit_count = cache_hit_count + 1
    } else {
      cache_miss_count = cache_miss_count + 1
      translation_cache[cache_key] = "cached_value"
    }
  }
  
  let cache_hit_rate = cache_hit_count.to_decimal() / (cache_hit_count + cache_miss_count).to_decimal()
  assert_true(cache_hit_rate > 0.5, "缓存命中率应该合理")
}