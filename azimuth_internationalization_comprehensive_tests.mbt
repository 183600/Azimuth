// Azimuth 国际化综合测试
// 全面测试多语言和本地化功能

// 测试1: 基本语言检测
test "基本语言检测" {
  // 支持的语言列表
  let supported_languages = ["en", "zh", "es", "fr", "de", "ja"]
  
  // 语言代码映射
  let language_names = {
    "en": "English",
    "zh": "中文",
    "es": "Español",
    "fr": "Français",
    "de": "Deutsch",
    "ja": "日本語"
  }
  
  // 检测语言是否支持
  let is_language_supported = fn(lang_code) {
    supported_languages.contains(lang_code)
  }
  
  // 获取语言名称
  let get_language_name = fn(lang_code) {
    if language_names.contains(lang_code) {
      language_names[lang_code]
    } else {
      "Unknown"
    }
  }
  
  // 测试语言检测
  assert_true(is_language_supported("en"))
  assert_true(is_language_supported("zh"))
  assert_false(is_language_supported("ru"))
  assert_false(is_language_supported("invalid"))
  
  assert_eq(get_language_name("en"), "English")
  assert_eq(get_language_name("zh"), "中文")
  assert_eq(get_language_name("invalid"), "Unknown")
}

// 测试2: 文本翻译
test "文本翻译" {
  // 翻译字典
  let translations = {
    "en": {
      "welcome": "Welcome",
      "login": "Login",
      "logout": "Logout",
      "username": "Username",
      "password": "Password",
      "submit": "Submit"
    },
    "zh": {
      "welcome": "欢迎",
      "login": "登录",
      "logout": "登出",
      "username": "用户名",
      "password": "密码",
      "submit": "提交"
    },
    "es": {
      "welcome": "Bienvenido",
      "login": "Iniciar sesión",
      "logout": "Cerrar sesión",
      "username": "Nombre de usuario",
      "password": "Contraseña",
      "submit": "Enviar"
    }
  }
  
  // 翻译函数
  let translate = fn(key, language) {
    if translations.contains(language) && translations[language].contains(key) {
      translations[language][key]
    } else if translations.contains("en") && translations["en"].contains(key) {
      // 回退到英语
      translations["en"][key]
    } else {
      "[" + key + "]" // 显示键名作为回退
    }
  }
  
  // 测试翻译
  assert_eq(translate("welcome", "en"), "Welcome")
  assert_eq(translate("welcome", "zh"), "欢迎")
  assert_eq(translate("welcome", "es"), "Bienvenido")
  
  assert_eq(translate("login", "zh"), "登录")
  assert_eq(translate("password", "es"), "Contraseña")
  
  // 测试回退机制
  assert_eq(translate("welcome", "fr"), "Welcome") // 回退到英语
  assert_eq(translate("nonexistent", "en"), "[nonexistent]") // 不存在的键
}

// 测试3: 复数形式处理
test "复数形式处理" {
  // 复数规则
  let plural_rules = {
    "en": fn(count) {
      if count == 1 { "one" } else { "other" }
    },
    "zh": fn(count) { "other" }, // 中文没有复数形式
    "ru": fn(count) {
      let mod10 = count % 10
      let mod100 = count % 100
      
      if mod10 == 1 && mod100 != 11 {
        "one"
      } else if mod10 >= 2 && mod10 <= 4 && (mod100 < 10 || mod100 >= 20) {
        "few"
      } else {
        "other"
      }
    }
  }
  
  // 复数翻译
  let plural_translations = {
    "en": {
      "item": {
        "one": "item",
        "other": "items"
      },
      "message": {
        "one": "message",
        "other": "messages"
      }
    },
    "zh": {
      "item": {
        "other": "项目"
      },
      "message": {
        "other": "消息"
      }
    },
    "ru": {
      "item": {
        "one": "элемент",
        "few": "элемента",
        "other": "элементов"
      }
    }
  }
  
  // 复数翻译函数
  let translate_plural = fn(key, count, language) {
    if plural_rules.contains(language) {
      let rule = plural_rules[language]
      let plural_form = rule(count)
      
      if plural_translations.contains(language) && 
         plural_translations[language].contains(key) && 
         plural_translations[language][key].contains(plural_form) {
        plural_translations[language][key][plural_form]
      } else {
        count.to_string() + " " + key
      }
    } else {
      count.to_string() + " " + key
    }
  }
  
  // 测试复数形式
  assert_eq(translate_plural("item", 1, "en"), "item")
  assert_eq(translate_plural("item", 5, "en"), "items")
  
  assert_eq(translate_plural("item", 1, "zh"), "项目")
  assert_eq(translate_plural("item", 5, "zh"), "项目") // 中文没有复数
  
  assert_eq(translate_plural("item", 1, "ru"), "элемент")
  assert_eq(translate_plural("item", 3, "ru"), "элемента")
  assert_eq(translate_plural("item", 5, "ru"), "элементов")
  assert_eq(translate_plural("item", 11, "ru"), "элементов") // 11是特殊情况
}

// 测试4: 日期和时间格式化
test "日期和时间格式化" {
  // 日期格式
  let date_formats = {
    "en": "MM/DD/YYYY",
    "zh": "YYYY年MM月DD日",
    "de": "DD.MM.YYYY",
    "ja": "YYYY年MM月DD日"
  }
  
  // 时间格式
  let time_formats = {
    "en": "h:mm:ss A",
    "zh": "HH:mm:ss",
    "de": "HH:mm:ss",
    "ja": "HH:mm:ss"
  }
  
  // 格式化日期
  let format_date = fn(year, month, day, language) {
    let format = if date_formats.contains(language) {
      date_formats[language]
    } else {
      "YYYY-MM-DD" // 默认格式
    }
    
    match format {
      "MM/DD/YYYY" => month.to_string() + "/" + day.to_string() + "/" + year.to_string(),
      "YYYY年MM月DD日" => year.to_string() + "年" + month.to_string() + "月" + day.to_string() + "日",
      "DD.MM.YYYY" => day.to_string() + "." + month.to_string() + "." + year.to_string(),
      _ => year.to_string() + "-" + month.to_string() + "-" + day.to_string()
    }
  }
  
  // 格式化时间
  let format_time = fn(hour, minute, second, language) {
    let format = if time_formats.contains(language) {
      time_formats[language]
    } else {
      "HH:mm:ss" // 默认格式
    }
    
    match format {
      "h:mm:ss A" => {
        let hour_12 = if hour > 12 { hour - 12 } else { if hour == 0 { 12 } else { hour } }
        let am_pm = if hour >= 12 { "PM" } else { "AM" }
        hour_12.to_string() + ":" + minute.to_string() + ":" + second.to_string() + " " + am_pm
      },
      _ => hour.to_string() + ":" + minute.to_string() + ":" + second.to_string()
    }
  }
  
  // 测试日期格式化
  assert_eq(format_date(2023, 1, 15, "en"), "1/15/2023")
  assert_eq(format_date(2023, 1, 15, "zh"), "2023年1月15日")
  assert_eq(format_date(2023, 1, 15, "de"), "15.1.2023")
  
  // 测试时间格式化
  assert_eq(format_time(14, 30, 45, "en"), "2:30:45 PM")
  assert_eq(format_time(14, 30, 45, "zh"), "14:30:45")
  assert_eq(format_time(9, 5, 30, "en"), "9:05:30 AM")
}

// 测试5: 数字格式化
test "数字格式化" {
  // 数字格式配置
  let number_formats = {
    "en": {
      decimal_separator: ".",
      thousands_separator: ",",
      currency_symbol: "$",
      currency_position: "before"
    },
    "de": {
      decimal_separator: ",",
      thousands_separator: ".",
      currency_symbol: "€",
      currency_position: "after"
    },
    "zh": {
      decimal_separator: ".",
      thousands_separator: ",",
      currency_symbol: "¥",
      currency_position: "before"
    }
  }
  
  // 格式化数字
  let format_number = fn(number, language) {
    if number_formats.contains(language) {
      let format = number_formats[language]
      let decimal_part = ""
      let integer_part = ""
      
      // 分离整数和小数部分
      let number_str = number.to_string()
      if number_str.contains(".") {
        let parts = number_str.split(".")
        integer_part = parts[0]
        decimal_part = format.decimal_separator + parts[1]
      } else {
        integer_part = number_str
      }
      
      // 添加千位分隔符
      let formatted_integer = ""
      let mut count = 0
      
      for i in integer_part.length() - 1 ..= 0 {
        formatted_integer = integer_part[i] + formatted_integer
        count = count + 1
        
        if count == 3 && i > 0 {
          formatted_integer = format.thousands_separator + formatted_integer
          count = 0
        }
      }
      
      formatted_integer + decimal_part
    } else {
      number.to_string()
    }
  }
  
  // 格式化货币
  let format_currency = fn(amount, language) {
    if number_formats.contains(language) {
      let format = number_formats[language]
      let formatted_number = format_number(amount, language)
      
      if format.currency_position == "before" {
        format.currency_symbol + formatted_number
      } else {
        formatted_number + " " + format.currency_symbol
      }
    } else {
      amount.to_string()
    }
  }
  
  // 测试数字格式化
  assert_eq(format_number(1234567.89, "en"), "1,234,567.89")
  assert_eq(format_number(1234567.89, "de"), "1.234.567,89")
  assert_eq(format_number(1234567.89, "zh"), "1,234,567.89")
  
  // 测试货币格式化
  assert_eq(format_currency(1234.56, "en"), "$1234.56")
  assert_eq(format_currency(1234.56, "de"), "1234,56 €")
  assert_eq(format_currency(1234.56, "zh"), "¥1234.56")
}

// 测试6: 文本方向和RTL支持
test "文本方向和RTL支持" {
  // 语言方向
  let language_directions = {
    "en": "ltr",
    "zh": "ltr",
    "ar": "rtl",
    "he": "rtl",
    "fa": "rtl"
  }
  
  // 获取文本方向
  let get_text_direction = fn(language) {
    if language_directions.contains(language) {
      language_directions[language]
    } else {
      "ltr" // 默认从左到右
    }
  }
  
  // 检查是否为RTL语言
  let is_rtl_language = fn(language) {
    get_text_direction(language) == "rtl"
  }
  
  // 调整文本对齐
  let adjust_text_align = fn(align, language) {
    if is_rtl_language(language) {
      match align {
        "left" => "right",
        "right" => "left",
        "center" => "center",
        _ => align
      }
    } else {
      align
    }
  }
  
  // 测试文本方向
  assert_eq(get_text_direction("en"), "ltr")
  assert_eq(get_text_direction("zh"), "ltr")
  assert_eq(get_text_direction("ar"), "rtl")
  assert_eq(get_text_direction("he"), "rtl")
  
  assert_false(is_rtl_language("en"))
  assert_false(is_rtl_language("zh"))
  assert_true(is_rtl_language("ar"))
  assert_true(is_rtl_language("he"))
  
  // 测试文本对齐调整
  assert_eq(adjust_text_align("left", "en"), "left")
  assert_eq(adjust_text_align("left", "ar"), "right")
  assert_eq(adjust_text_align("right", "ar"), "left")
  assert_eq(adjust_text_align("center", "he"), "center")
}

// 测试7: 时区处理
test "时区处理" {
  // 时区偏移量（小时）
  let timezone_offsets = {
    "UTC": 0,
    "EST": -5,
    "PST": -8,
    "CST": 8, // 中国标准时间
    "JST": 9, // 日本标准时间
    "CET": 1  // 中欧时间
  }
  
  // 转换时区
  let convert_timezone = fn(timestamp, from_tz, to_tz) {
    if timezone_offsets.contains(from_tz) && timezone_offsets.contains(to_tz) {
      let from_offset = timezone_offsets[from_tz]
      let to_offset = timezone_offsets[to_tz]
      let offset_diff = to_offset - from_offset
      
      timestamp + (offset_diff * 3600) // 转换为秒
    } else {
      timestamp
    }
  }
  
  // 格式化时间戳为可读时间
  let format_timestamp = fn(timestamp) {
    let hours = (timestamp / 3600) % 24
    let minutes = (timestamp % 3600) / 60
    let seconds = timestamp % 60
    
    hours.to_string() + ":" + 
    (if minutes < 10 { "0" } else { "" }) + minutes.to_string() + ":" +
    (if seconds < 10 { "0" } else { "" }) + seconds.to_string()
  }
  
  // 测试时区转换
  let utc_time = 12 * 3600 // 12:00:00 UTC
  
  let est_time = convert_timezone(utc_time, "UTC", "EST")
  let cst_time = convert_timezone(utc_time, "UTC", "CST")
  let jst_time = convert_timezone(utc_time, "UTC", "JST")
  
  assert_eq(format_timestamp(est_time), "7:00:00") // UTC-5
  assert_eq(format_timestamp(cst_time), "20:00:00") // UTC+8
  assert_eq(format_timestamp(jst_time), "21:00:00") // UTC+9
  
  // 测试时区之间转换
  let est_to_cst = convert_timezone(est_time, "EST", "CST")
  assert_eq(format_timestamp(est_to_cst), "20:00:00")
}

// 测试8: 本地化排序
test "本地化排序" {
  // 本地化排序规则
  let localized_sort = fn(strings, language) {
    match language {
      "zh" => {
        // 中文按拼音排序（简化版）
        strings.sort(fn(a, b) { a < b })
      }
      "de" => {
        // 德语特殊字符排序（简化版）
        strings.sort(fn(a, b) {
          // 将ä替换为ae，ö替换为oe，ü替换为ue
          let normalize = fn(s) {
            s.replace("ä", "ae").replace("ö", "oe").replace("ü", "ue")
          }
          normalize(a) < normalize(b)
        })
      }
      _ => {
        // 默认排序
        strings.sort(fn(a, b) { a < b })
      }
    }
  }
  
  // 测试中文排序
  let chinese_names = ["张三", "李四", "王五", "赵六"]
  let sorted_chinese = localized_sort(chinese_names, "zh")
  
  // 测试德语排序
  let german_words = ["äpfel", "öfen", "übung", "banana"]
  let sorted_german = localized_sort(german_words, "de")
  
  // 测试英文排序
  let english_words = ["apple", "banana", "cherry", "date"]
  let sorted_english = localized_sort(english_words, "en")
  
  // 验证排序结果
  assert_eq(sorted_chinese.length(), 4)
  assert_eq(sorted_german.length(), 4)
  assert_eq(sorted_english.length(), 4)
  
  // 英文排序验证
  assert_eq(sorted_english[0], "apple")
  assert_eq(sorted_english[1], "banana")
  assert_eq(sorted_english[2], "cherry")
  assert_eq(sorted_english[3], "date")
}

// 测试9: 语言回退机制
test "语言回退机制" {
  // 支持的语言层次结构
  let language_hierarchy = {
    "zh-CN": ["zh-CN", "zh", "en"],
    "zh-TW": ["zh-TW", "zh", "en"],
    "en-US": ["en-US", "en"],
    "es-ES": ["es-ES", "es", "en"],
    "fr-FR": ["fr-FR", "fr", "en"]
  }
  
  // 可用翻译
  let available_translations = ["en", "zh", "es"]
  
  // 查找最佳匹配语言
  let find_best_language = fn(preferred) {
    if language_hierarchy.contains(preferred) {
      let hierarchy = language_hierarchy[preferred]
      
      for lang in hierarchy {
        if available_translations.contains(lang) {
          return lang
        }
      }
    }
    
    // 默认回退到英语
    "en"
  }
  
  // 测试语言回退
  assert_eq(find_best_language("zh-CN"), "zh") // zh-CN不可用，回退到zh
  assert_eq(find_best_language("zh-TW"), "zh") // zh-TW不可用，回退到zh
  assert_eq(find_best_language("en-US"), "en") // en-US不可用，回退到en
  assert_eq(find_best_language("es-ES"), "es") // es-ES不可用，回退到es
  assert_eq(find_best_language("fr-FR"), "en") // fr和fr-FR都不可用，回退到en
  assert_eq(find_best_language("ja-JP"), "en") // 不在层次结构中，回退到en
}

// 测试10: 字符编码处理
test "字符编码处理" {
  // 不同语言的文本样本
  let text_samples = {
    "en": "Hello, World!",
    "zh": "你好，世界！",
    "ja": "こんにちは、世界！",
    "ar": "مرحبا بالعالم!",
    "ru": "Привет, мир!",
    "emoji": "Hello 🌍🚀👋"
  }
  
  // 计算字符长度（考虑Unicode）
  let unicode_length = fn(text) {
    let mut count = 0
    let mut i = 0
    
    while i < text.length() {
      let ch = text[i]
      
      // 简化的Unicode字符长度计算
      if ch >= '\u{D800}' && ch <= '\u{DFFF}' {
        // 代理对（UTF-16）
        i = i + 2
      } else {
        i = i + 1
      }
      
      count = count + 1
    }
    
    count
  }
  
  // 检测是否包含非ASCII字符
  let has_non_ascii = fn(text) {
    for ch in text.to_char_array() {
      if ch.to_int() > 127 {
        return true
      }
    }
    false
  }
  
  // 检测是否包含表情符号
  let has_emoji = fn(text) {
    // 简化的表情符号检测
    text.contains("🌍") || text.contains("🚀") || text.contains("👋") || 
    text.contains("😀") || text.contains("❤️")
  }
  
  // 测试字符编码处理
  assert_eq(unicode_length(text_samples["en"]), 13)
  assert_eq(unicode_length(text_samples["zh"]), 6) // 中文字符
  assert_eq(unicode_length(text_samples["ja"]), 9) // 日文字符
  assert_eq(unicode_length(text_samples["ar"]), 14) // 阿拉伯字符
  assert_eq(unicode_length(text_samples["ru"]), 12) // 西里尔字符
  assert_eq(unicode_length(text_samples["emoji"]), 11) // 包含表情符号
  
  assert_false(has_non_ascii(text_samples["en"]))
  assert_true(has_non_ascii(text_samples["zh"]))
  assert_true(has_non_ascii(text_samples["ja"]))
  assert_true(has_non_ascii(text_samples["ar"]))
  assert_true(has_non_ascii(text_samples["ru"]))
  assert_true(has_non_ascii(text_samples["emoji"]))
  
  assert_false(has_emoji(text_samples["en"]))
  assert_false(has_emoji(text_samples["zh"]))
  assert_true(has_emoji(text_samples["emoji"]))
}