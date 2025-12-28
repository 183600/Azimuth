// 国际化支持高级测试
// 测试多语言支持、Unicode处理、本地化格式和全球化功能

test "多语言字符串处理测试" {
  // 测试中文支持
  let chinese_string = StringValue("这是一个测试字符串，包含中文字符")
  match chinese_string {
    StringValue(s) => {
      assert_true(s.contains("中文"), "中文字符串应该包含'中文'")
      assert_true(s.contains("测试"), "中文字符串应该包含'测试'")
      assert_eq(s.length(), 16, "中文字符串长度应该正确")
    }
    _ => assert_true(false, "期望字符串值")
  }
  
  // 测试日文支持
  let japanese_string = StringValue("これはテスト文字列で、日本語文字が含まれています")
  match japanese_string {
    StringValue(s) => {
      assert_true(s.contains("日本語"), "日文字符串应该包含'日本語'")
      assert_true(s.contains("テスト"), "日文字符串应该包含'テスト'")
      assert_eq(s.length(), 24, "日文字符串长度应该正确")
    }
    _ => assert_true(false, "期望字符串值")
  }
  
  // 测试韩文支持
  let korean_string = StringValue("이것은 테스트 문자열이며, 한국어 문자가 포함되어 있습니다")
  match korean_string {
    StringValue(s) => {
      assert_true(s.contains("한국어"), "韩文字符串应该包含'한국어'")
      assert_true(s.contains("테스트"), "韩文字符串应该包含'테스트'")
      assert_eq(s.length(), 27, "韩文字符串长度应该正确")
    }
    _ => assert_true(false, "期望字符串值")
  }
  
  // 测试阿拉伯文支持
  let arabic_string = StringValue("هذه سلسلة اختبار تحتوي على أحرف عربية")
  match arabic_string {
    StringValue(s) => {
      assert_true(s.contains("عربية"), "阿拉伯文字符串应该包含'عربية'")
      assert_true(s.contains("اختبار"), "阿拉伯文字符串应该包含'اختبار'")
      assert_eq(s.length(), 31, "阿拉伯文字符串长度应该正确")
    }
    _ => assert_true(false, "期望字符串值")
  }
  
  // 测试俄文支持
  let russian_string = StringValue("Это тестовая строка, содержащая русские символы")
  match russian_string {
    StringValue(s) => {
      assert_true(s.contains("русские"), "俄文字符串应该包含'русские'")
      assert_true(s.contains("тестовая"), "俄文字符串应该包含'тестовая'")
      assert_eq(s.length(), 36, "俄文字符串长度应该正确")
    }
    _ => assert_true(false, "期望字符串值")
  }
  
  // 测试希伯来文支持
  let hebrew_string = StringValue("זוהי מחרוזת בדיקה המכילה תווים בעברית")
  match hebrew_string {
    StringValue(s) => {
      assert_true(s.contains("עברית"), "希伯来文字符串应该包含'עברית'")
      assert_true(s.contains("בדיקה"), "希伯来文字符串应该包含'בדיקה'")
      assert_eq(s.length(), 27, "希伯来文字符串长度应该正确")
    }
    _ => assert_true(false, "期望字符串值")
  }
  
  // 测试泰文支持
  let thai_string = StringValue("นี่คือสตริงทดสอบที่มีอักขระภาษาไทย")
  match thai_string {
    StringValue(s) => {
      assert_true(s.contains("ภาษาไทย"), "泰文字符串应该包含'ภาษาไทย'")
      assert_true(s.contains("ทดสอบ"), "泰文字符串应该包含'ทดสอบ'")
      assert_eq(s.length(), 30, "泰文字符串长度应该正确")
    }
    _ => assert_true(false, "期望字符串值")
  }
}

test "Unicode字符和Emoji支持测试" {
  // 测试基本Emoji支持
  let basic_emoji_string = StringValue("Hello World! 🌍🚀⭐💡")
  match basic_emoji_string {
    StringValue(s) => {
      assert_true(s.contains("🌍"), "字符串应该包含地球emoji")
      assert_true(s.contains("🚀"), "字符串应该包含火箭emoji")
      assert_true(s.contains("⭐"), "字符串应该包含星星emoji")
      assert_true(s.contains("💡"), "字符串应该包含灯泡emoji")
      assert_eq(s.length(), 18, "基本emoji字符串长度应该正确")
    }
    _ => assert_true(false, "期望字符串值")
  }
  
  // 测试复杂Emoji支持
  let complex_emoji_string = StringValue("Complex emojis: 👨‍👩‍👧‍👦 🏳️‍🌈 🎉🎊🎈")
  match complex_emoji_string {
    StringValue(s) => {
      assert_true(s.contains("👨‍👩‍👧‍👦"), "字符串应该包含家庭emoji")
      assert_true(s.contains("🏳️‍🌈"), "字符串应该包含彩虹旗emoji")
      assert_true(s.contains("🎉"), "字符串应该包含派对emoji")
      assert_eq(s.length(), 28, "复杂emoji字符串长度应该正确")
    }
    _ => assert_true(false, "期望字符串值")
  }
  
  // 测试Unicode特殊字符
  let unicode_special_string = StringValue("Special Unicode: ©®™€£¥§¶†‡•…‰‹›""''–—")
  match unicode_special_string {
    StringValue(s) => {
      assert_true(s.contains("©"), "字符串应该包含版权符号")
      assert_true(s.contains("®"), "字符串应该包含注册商标符号")
      assert_true(s.contains("™"), "字符串应该包含商标符号")
      assert_true(s.contains("€"), "字符串应该包含欧元符号")
      assert_true(s.contains("£"), "字符串应该包含英镑符号")
      assert_true(s.contains("¥"), "字符串应该包含日元符号")
      assert_eq(s.length(), 32, "Unicode特殊字符字符串长度应该正确")
    }
    _ => assert_true(false, "期望字符串值")
  }
  
  // 测试数学符号
  let math_symbols_string = StringValue("Math symbols: ∑∏∫∆∇∂∞±×÷≤≥≠≈∈∉⊂⊃∧∨¬")
  match math_symbols_string {
    StringValue(s) => {
      assert_true(s.contains("∑"), "字符串应该包含求和符号")
      assert_true(s.contains("∞"), "字符串应该包含无穷符号")
      assert_true(s.contains("±"), "字符串应该包含正负号")
      assert_true(s.contains("×"), "字符串应该包含乘号")
      assert_true(s.contains("÷"), "字符串应该包含除号")
      assert_eq(s.length(), 32, "数学符号字符串长度应该正确")
    }
    _ => assert_true(false, "期望字符串值")
  }
  
  // 测试箭头符号
  let arrows_string = StringValue("Arrows: ←→↑↓↖↗↘↙⇐⇒⇑⇓⇔⇕⇖⇗⇘⇙")
  match arrows_string {
    StringValue(s) => {
      assert_true(s.contains("←"), "字符串应该包含左箭头")
      assert_true(s.contains("→"), "字符串应该包含右箭头")
      assert_true(s.contains("↑"), "字符串应该包含上箭头")
      assert_true(s.contains("↓"), "字符串应该包含下箭头")
      assert_eq(s.length(), 26, "箭头符号字符串长度应该正确")
    }
    _ => assert_true(false, "期望字符串值")
  }
}

test "多语言日志记录测试" {
  // 创建多语言日志提供者
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "internationalization-test")
  
  // 创建中文日志记录
  let chinese_log = LogRecord::new_with_context(
    Info,
    Some("用户登录成功，用户ID: 12345"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-cn-12345"),
    Some("span-cn-67890"),
    Some(Context::with_value(Context::root(), ContextKey::new("language"), "zh-CN"))
  )
  
  // 创建日文日志记录
  let japanese_log = LogRecord::new_with_context(
    Warn,
    Some("データベース接続タイムアウトが発生しました"),
    None,
    Some(1735689600000002000L),
    Some(1735689600000003000L),
    Some("trace-ja-12345"),
    Some("span-ja-67890"),
    Some(Context::with_value(Context::root(), ContextKey::new("language"), "ja-JP"))
  )
  
  // 创建韩文日志记录
  let korean_log = LogRecord::new_with_context(
    Error,
    Some("서버 오류가 발생했습니다. 관리자에게 문의하세요."),
    None,
    Some(1735689600000004000L),
    Some(1735689600000005000L),
    Some("trace-ko-12345"),
    Some("span-ko-67890"),
    Some(Context::with_value(Context::root(), ContextKey::new("language"), "ko-KR"))
  )
  
  // 发送多语言日志
  Logger::emit(logger, chinese_log)
  Logger::emit(logger, japanese_log)
  Logger::emit(logger, korean_log)
  
  // 验证中文日志
  assert_eq(LogRecord::severity_number(chinese_log), Info, "中文日志严重级别应该正确")
  match LogRecord::body(chinese_log) {
    Some(body) => {
      assert_true(body.contains("用户登录"), "中文日志应该包含用户登录信息")
      assert_true(body.contains("12345"), "中文日志应该包含用户ID")
    }
    _ => assert_true(false, "期望中文日志有内容")
  }
  assert_eq(LogRecord::trace_id(chinese_log), Some("trace-cn-12345"), "中文日志追踪ID应该正确")
  
  // 验证日文日志
  assert_eq(LogRecord::severity_number(japanese_log), Warn, "日文日志严重级别应该正确")
  match LogRecord::body(japanese_log) {
    Some(body) => {
      assert_true(body.contains("データベース"), "日文日志应该包含数据库信息")
      assert_true(body.contains("タイムアウト"), "日文日志应该包含超时信息")
    }
    _ => assert_true(false, "期望日文日志有内容")
  }
  assert_eq(LogRecord::trace_id(japanese_log), Some("trace-ja-12345"), "日文日志追踪ID应该正确")
  
  // 验证韩文日志
  assert_eq(LogRecord::severity_number(korean_log), Error, "韩文日志严重级别应该正确")
  match LogRecord::body(korean_log) {
    Some(body) => {
      assert_true(body.contains("서버 오류"), "韩文日志应该包含服务器错误信息")
      assert_true(body.contains("관리자"), "韩文日志应该包含管理员信息")
    }
    _ => assert_true(false, "期望韩文日志有内容")
  }
  assert_eq(LogRecord::trace_id(korean_log), Some("trace-ko-12345"), "韩文日志追踪ID应该正确")
}

test "多语言属性和资源测试" {
  // 创建多语言属性
  let multilingual_attributes = Attributes::new()
  
  // 设置不同语言的属性值
  Attributes::set(multilingual_attributes, "error.message.zh", StringValue("操作失败，请重试"))
  Attributes::set(multilingual_attributes, "error.message.ja", StringValue("操作が失敗しました。再試行してください。"))
  Attributes::set(multilingual_attributes, "error.message.ko", StringValue("작업이 실패했습니다. 다시 시도하세요."))
  Attributes::set(multilingual_attributes, "error.message.en", StringValue("Operation failed, please retry"))
  Attributes::set(multilingual_attributes, "error.message.ar", StringValue("فشلت العملية، يرجى المحاولة مرة أخرى"))
  
  // 验证多语言属性
  let zh_message = Attributes::get(multilingual_attributes, "error.message.zh")
  let ja_message = Attributes::get(multilingual_attributes, "error.message.ja")
  let ko_message = Attributes::get(multilingual_attributes, "error.message.ko")
  let en_message = Attributes::get(multilingual_attributes, "error.message.en")
  let ar_message = Attributes::get(multilingual_attributes, "error.message.ar")
  
  match zh_message {
    Some(StringValue(msg)) => {
      assert_true(msg.contains("操作失败"), "中文错误消息应该正确")
      assert_true(msg.contains("请重试"), "中文错误消息应该包含重试提示")
    }
    _ => assert_true(false, "期望中文错误消息")
  }
  
  match ja_message {
    Some(StringValue(msg)) => {
      assert_true(msg.contains("失敗"), "日文错误消息应该正确")
      assert_true(msg.contains("再試行"), "日文错误消息应该包含重试提示")
    }
    _ => assert_true(false, "期望日文错误消息")
  }
  
  match ko_message {
    Some(StringValue(msg)) => {
      assert_true(msg.contains("실패"), "韩文错误消息应该正确")
      assert_true(msg.contains("다시 시도"), "韩文错误消息应该包含重试提示")
    }
    _ => assert_true(false, "期望韩文错误消息")
  }
  
  match en_message {
    Some(StringValue(msg)) => {
      assert_true(msg.contains("failed"), "英文错误消息应该正确")
      assert_true(msg.contains("retry"), "英文错误消息应该包含重试提示")
    }
    _ => assert_true(false, "期望英文错误消息")
  }
  
  match ar_message {
    Some(StringValue(msg)) => {
      assert_true(msg.contains("فشلت"), "阿拉伯文错误消息应该正确")
      assert_true(msg.contains("المحاولة"), "阿拉伯文错误消息应该包含重试提示")
    }
    _ => assert_true(false, "期望阿拉伯文错误消息")
  }
  
  // 创建多语言资源
  let multilingual_resource = Resource::new()
  let multilingual_resource_attributes = [
    ("service.name.zh", StringValue("用户服务")),
    ("service.name.ja", StringValue("ユーザーサービス")),
    ("service.name.ko", StringValue("사용자 서비스")),
    ("service.name.en", StringValue("User Service")),
    ("service.description.zh", StringValue("处理用户注册、登录和个人信息管理")),
    ("service.description.ja", StringValue("ユーザー登録、ログイン、個人情報管理を処理")),
    ("service.description.ko", StringValue("사용자 등록, 로그인, 개인 정보 관리를 처리")),
    ("service.description.en", StringValue("Handles user registration, login, and profile management")),
    ("region.zh", StringValue("亚太地区")),
    ("region.ja", StringValue("アジア太平洋地域")),
    ("region.ko", StringValue("아시아 태평양 지역")),
    ("region.en", StringValue("Asia Pacific"))
  ]
  let enriched_multilingual_resource = Resource::with_attributes(multilingual_resource, multilingual_resource_attributes)
  
  // 验证多语言资源
  let service_name_zh = Resource::get_attribute(enriched_multilingual_resource, "service.name.zh")
  let service_name_ja = Resource::get_attribute(enriched_multilingual_resource, "service.name.ja")
  let service_name_ko = Resource::get_attribute(enriched_multilingual_resource, "service.name.ko")
  let service_name_en = Resource::get_attribute(enriched_multilingual_resource, "service.name.en")
  
  match service_name_zh {
    Some(StringValue(name)) => assert_eq(name, "用户服务", "中文服务名称应该正确")
    _ => assert_true(false, "期望中文服务名称")
  }
  
  match service_name_ja {
    Some(StringValue(name)) => assert_eq(name, "ユーザーサービス", "日文服务名称应该正确")
    _ => assert_true(false, "期望日文服务名称")
  }
  
  match service_name_ko {
    Some(StringValue(name)) => assert_eq(name, "사용자 서비스", "韩文服务名称应该正确")
    _ => assert_true(false, "期望韩文服务名称")
  }
  
  match service_name_en {
    Some(StringValue(name)) => assert_eq(name, "User Service", "英文服务名称应该正确")
    _ => assert_true(false, "期望英文服务名称")
  }
}

test "本地化格式和时区测试" {
  // 测试不同地区的日期时间格式（模拟）
  let us_date_string = StringValue("12/28/2025 03:45:30 PM EST")
  let eu_date_string = StringValue("28/12/2025 15:45:30 CET")
  let iso_date_string = StringValue("2025-12-28T15:45:30+00:00")
  let cn_date_string = StringValue("2025年12月28日 15:45:30 CST")
  let jp_date_string = StringValue("2025年12月28日 15時45分30秒 JST")
  
  // 验证不同地区的日期格式
  match us_date_string {
    StringValue(s) => {
      assert_true(s.contains("12/28/2025"), "美式日期格式应该正确")
      assert_true(s.contains("PM"), "美式时间应该包含AM/PM")
      assert_true(s.contains("EST"), "美式时区应该正确")
    }
    _ => assert_true(false, "期望美式日期字符串")
  }
  
  match eu_date_string {
    StringValue(s) => {
      assert_true(s.contains("28/12/2025"), "欧式日期格式应该正确")
      assert_true(s.contains("15:45"), "欧式时间应该是24小时制")
      assert_true(s.contains("CET"), "欧式时区应该正确")
    }
    _ => assert_true(false, "期望欧式日期字符串")
  }
  
  match iso_date_string {
    StringValue(s) => {
      assert_true(s.contains("2025-12-28T"), "ISO日期格式应该正确")
      assert_true(s.contains("+00:00"), "ISO时区应该正确")
    }
    _ => assert_true(false, "期望ISO日期字符串")
  }
  
  match cn_date_string {
    StringValue(s) => {
      assert_true(s.contains("2025年12月28日"), "中文日期格式应该正确")
      assert_true(s.contains("CST"), "中文时区应该正确")
    }
    _ => assert_true(false, "期望中文日期字符串")
  }
  
  match jp_date_string {
    StringValue(s) => {
      assert_true(s.contains("2025年12月28日"), "日文日期格式应该正确")
      assert_true(s.contains("15時45分"), "日文时间格式应该正确")
      assert_true(s.contains("JST"), "日文时区应该正确")
    }
    _ => assert_true(false, "期望日文日期字符串")
  }
  
  // 测试数字格式
  let us_number_string = StringValue("1,234,567.89")
  let eu_number_string = StringValue("1.234.567,89")
  let cn_number_string = StringValue("1,234,567.89")
  let in_number_string = StringValue("12,34,567.89")
  
  match us_number_string {
    StringValue(s) => {
      assert_true(s.contains("1,234,567"), "美式数字千位分隔符应该正确")
      assert_true(s.contains(".89"), "美式数字小数点应该正确")
    }
    _ => assert_true(false, "期望美式数字字符串")
  }
  
  match eu_number_string {
    StringValue(s) => {
      assert_true(s.contains("1.234.567"), "欧式数字千位分隔符应该正确")
      assert_true(s.contains(",89"), "欧式数字小数点应该正确")
    }
    _ => assert_true(false, "期望欧式数字字符串")
  }
  
  // 测试货币格式
  let us_currency_string = StringValue("$1,234.56")
  let eu_currency_string = StringValue("€1.234,56")
  let jp_currency_string = StringValue("¥1,235")
  let cn_currency_string = StringValue("¥1,234.56")
  
  match us_currency_string {
    StringValue(s) => {
      assert_true(s.contains("$"), "美元符号应该正确")
      assert_true(s.contains("1,234.56"), "美元格式应该正确")
    }
    _ => assert_true(false, "期望美元字符串")
  }
  
  match eu_currency_string {
    StringValue(s) => {
      assert_true(s.contains("€"), "欧元符号应该正确")
      assert_true(s.contains("1.234,56"), "欧元格式应该正确")
    }
    _ => assert_true(false, "期望欧元字符串")
  }
  
  match jp_currency_string {
    StringValue(s) => {
      assert_true(s.contains("¥"), "日元符号应该正确")
      assert_true(s.contains("1,235"), "日元格式应该正确")
    }
    _ => assert_true(false, "期望日元字符串")
  }
  
  match cn_currency_string {
    StringValue(s) => {
      assert_true(s.contains("¥"), "人民币符号应该正确")
      assert_true(s.contains("1,234.56"), "人民币格式应该正确")
    }
    _ => assert_true(false, "期望人民币字符串")
  }
}

test "文化适应性测试" {
  // 测试从右到左（RTL）语言
  let rtl_string = StringValue("مرحبا بالعالم! هذه رسالة باللغة العربية.")
  match rtl_string {
    StringValue(s) => {
      assert_true(s.contains("مرحبا"), "RTL字符串应该包含阿拉伯文问候")
      assert_true(s.contains("العربية"), "RTL字符串应该包含阿拉伯文标识")
      assert_eq(s.length(), 39, "RTL字符串长度应该正确")
    }
    _ => assert_true(false, "期望RTL字符串")
  }
  
  // 测试希伯来文（RTL）
  let hebrew_string = StringValue("שלום עולם! זו הודעה בשפה העברית.")
  match hebrew_string {
    StringValue(s) => {
      assert_true(s.contains("שלום"), "希伯来文字符串应该包含问候")
      assert_true(s.contains("עברית"), "希伯来文字符串应该包含语言标识")
      assert_eq(s.length(), 30, "希伯来文字符串长度应该正确")
    }
    _ => assert_true(false, "期望希伯来文字符串")
  }
  
  // 测试复合语言环境
  let mixed_locale_string = StringValue("User: 用户名123, Region: 中国, Language: 中文(简体)")
  match mixed_locale_string {
    StringValue(s) => {
      assert_true(s.contains("用户名123"), "混合语言字符串应该包含中文用户名")
      assert_true(s.contains("中国"), "混合语言字符串应该包含中文地区")
      assert_true(s.contains("中文(简体)"), "混合语言字符串应该包含语言标识")
      assert_true(s.contains("User:"), "混合语言字符串应该包含英文标签")
      assert_true(s.contains("Region:"), "混合语言字符串应该包含英文标签")
      assert_true(s.contains("Language:"), "混合语言字符串应该包含英文标签")
    }
    _ => assert_true(false, "期望混合语言字符串")
  }
  
  // 创建多语言上下文
  let zh_context = Context::with_value(Context::root(), ContextKey::new("locale"), "zh-CN")
  let ja_context = Context::with_value(Context::root(), ContextKey::new("locale"), "ja-JP")
  let ko_context = Context::with_value(Context::root(), ContextKey::new("locale"), "ko-KR")
  let ar_context = Context::with_value(Context::root(), ContextKey::new("locale"), "ar-SA")
  let he_context = Context::with_value(Context::root(), ContextKey::new("locale"), "he-IL")
  
  // 验证多语言上下文
  let zh_locale = Context::get(zh_context, ContextKey::new("locale"))
  let ja_locale = Context::get(ja_context, ContextKey::new("locale"))
  let ko_locale = Context::get(ko_context, ContextKey::new("locale"))
  let ar_locale = Context::get(ar_context, ContextKey::new("locale"))
  let he_locale = Context::get(he_context, ContextKey::new("locale"))
  
  assert_eq(zh_locale, Some("zh-CN"), "中文语言环境应该正确")
  assert_eq(ja_locale, Some("ja-JP"), "日文语言环境应该正确")
  assert_eq(ko_locale, Some("ko-KR"), "韩文语言环境应该正确")
  assert_eq(ar_locale, Some("ar-SA"), "阿拉伯文语言环境应该正确")
  assert_eq(he_locale, Some("he-IL"), "希伯来文语言环境应该正确")
  
  // 测试文化相关的度量名称
  let zh_meter = MeterProvider::get_meter(MeterProvider::default(), "中文度量")
  let ja_meter = MeterProvider::get_meter(MeterProvider::default(), "日本語メトリック")
  let ko_meter = MeterProvider::get_meter(MeterProvider::default(), "한국어 메트릭")
  
  let zh_counter = Meter::create_counter(zh_meter, "中文计数器")
  let ja_counter = Meter::create_counter(ja_meter, "日本語カウンター")
  let ko_counter = Meter::create_counter(ko_meter, "한국어 카운터")
  
  // 验证文化相关的度量
  assert_eq(zh_counter.name, "中文计数器", "中文计数器名称应该正确")
  assert_eq(ja_counter.name, "日本語カウンター", "日文计数器名称应该正确")
  assert_eq(ko_counter.name, "한국어 카운터", "韩文计数器名称应该正确")
  
  // 记录文化相关的度量数据
  Counter::add(zh_counter, 100.0)
  Counter::add(ja_counter, 200.0)
  Counter::add(ko_counter, 300.0)
}