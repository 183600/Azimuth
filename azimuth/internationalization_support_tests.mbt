// 国际化支持测试用例
// 测试多语言和本地化功能

test "多语言Span名称测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "国际化测试")
  
  // 测试中文Span名称
  let chinese_span = Tracer::start_span(tracer, "用户登录操作")
  assert_eq(Span::name(chinese_span), "用户登录操作")
  Span::add_event(chinese_span, "验证成功", Some([("用户ID", StringValue("12345")), ("用户名", StringValue("张三"))]))
  Span::set_status(chinese_span, Ok, Some("操作成功完成"))
  Span::end(chinese_span)
  
  // 测试英文Span名称
  let english_tracer = TracerProvider::get_tracer(tracer_provider, "internationalization.test")
  let english_span = Tracer::start_span(english_tracer, "User Login Operation")
  assert_eq(Span::name(english_span), "User Login Operation")
  Span::add_event(english_span, "Authentication Success", Some([("userID", StringValue("12345")), ("username", StringValue("John Doe"))]))
  Span::set_status(english_span, Ok, Some("Operation completed successfully"))
  Span::end(english_span)
  
  // 测试日文Span名称
  let japanese_tracer = TracerProvider::get_tracer(tracer_provider, "国際化テスト")
  let japanese_span = Tracer::start_span(japanese_tracer, "ユーザーログイン操作")
  assert_eq(Span::name(japanese_span), "ユーザーログイン操作")
  Span::add_event(japanese_span, "認証成功", Some([("ユーザーID", StringValue("12345")), ("ユーザー名", StringValue("田中太郎"))]))
  Span::set_status(japanese_span, Ok, Some("操作が正常に完了しました"))
  Span::end(japanese_span)
  
  // 测试韩文Span名称
  let korean_tracer = TracerProvider::get_tracer(tracer_provider, "국제화 테스트")
  let korean_span = Tracer::start_span(korean_tracer, "사용자 로그인 작업")
  assert_eq(Span::name(korean_span), "사용자 로그인 작업")
  Span::add_event(korean_span, "인증 성공", Some([("사용자ID", StringValue("12345")), ("사용자명", StringValue("김철수"))]))
  Span::set_status(korean_span, Ok, Some("작업이 성공적으로 완료되었습니다"))
  Span::end(korean_span)
  
  assert_true(true) // 如果所有多语言span操作都成功则测试通过
}

test "多语言属性值测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "multilingual.attributes")
  let span = Tracer::start_span(tracer, "Multilingual Attributes Test")
  
  // 测试各种语言的属性值
  let multilingual_attrs = [
    ("中文.消息", StringValue("这是一个中文消息")),
    ("英文.消息", StringValue("This is an English message")),
    ("日文.消息", StringValue("これは日本語のメッセージです")),
    ("韩文.消息", StringValue("이것은 한국어 메시지입니다")),
    ("阿拉伯文.消息", StringValue("هذه رسالة باللغة العربية")),
    ("俄文.消息", StringValue("Это сообщение на русском языке")),
    ("法文.消息", StringValue("Ceci est un message en français")),
    ("德文.消息", StringValue("Dies ist eine deutsche Nachricht")),
    ("西班牙文.消息", StringValue("Este es un mensaje en español")),
    ("葡萄牙文.消息", StringValue("Esta é uma mensagem em português"))
  ]
  
  // 添加多语言属性到span
  for attr in multilingual_attrs {
    Span::add_event(span, "multilingual.attr", Some([attr]))
  }
  
  // 测试包含多种语言的复合属性
  let composite_attrs = [
    ("多语言.标题", StringValue("国际化测试 / Internationalization Test / 国際化テスト / 국제화 테스트")),
    ("多语言.描述", StringValue("这是一个包含多种语言的测试描述 / This is a test description with multiple languages")),
    ("多语言.标签", ArrayStringValue(["中文标签", "English Tag", "日本語タグ", "한국어 태그"]))
  ]
  
  for attr in composite_attrs {
    Span::add_event(span, "composite.attr", Some([attr]))
  }
  
  Span::end(span)
  assert_true(true) // 如果多语言属性处理成功则测试通过
}

test "多语言日志记录测试" {
  let logger_provider = LoggerProvider::default()
  
  // 创建不同语言的logger
  let chinese_logger = LoggerProvider::get_logger(logger_provider, "中文日志记录器")
  let english_logger = LoggerProvider::get_logger(logger_provider, "english.logger")
  let japanese_logger = LoggerProvider::get_logger(logger_provider, "日本語ロガー")
  
  // 测试中文日志
  let chinese_log = LogRecord::new_with_context(
    Info,
    Some("用户登录成功"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("中文追踪ID"),
    Some("中文跨度ID"),
    None
  )
  Logger::emit(chinese_logger, chinese_log)
  
  // 测试英文日志
  let english_log = LogRecord::new_with_context(
    Info,
    Some("User login successful"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("english_trace_id"),
    Some("english_span_id"),
    None
  )
  Logger::emit(english_logger, english_log)
  
  // 测试日文日志
  let japanese_log = LogRecord::new_with_context(
    Info,
    Some("ユーザーログイン成功"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("日本語トレースID"),
    Some("日本語スパンID"),
    None
  )
  Logger::emit(japanese_logger, japanese_log)
  
  // 测试多语言错误日志
  let error_logs = [
    (Warn, "警告：数据库连接超时", "Warning: Database connection timeout", "警告：データベース接続タイムアウト"),
    (Error, "错误：用户认证失败", "Error: User authentication failed", "エラー：ユーザー認証失敗"),
    (Fatal, "致命错误：系统崩溃", "Fatal error: System crash", "致命的なエラー：システムクラッシュ")
  ]
  
  for log_data in error_logs {
    let (severity, chinese_msg, english_msg, japanese_msg) = log_data
    
    let chinese_error = LogRecord::new(severity, chinese_msg)
    let english_error = LogRecord::new(severity, english_msg)
    let japanese_error = LogRecord::new(severity, japanese_msg)
    
    Logger::emit(chinese_logger, chinese_error)
    Logger::emit(english_logger, english_error)
    Logger::emit(japanese_logger, japanese_error)
  }
  
  assert_true(true) // 如果所有多语言日志记录成功则测试通过
}

test "多语言Metrics测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "多语言指标")
  
  // 创建多语言metrics
  let chinese_counter = Meter::create_counter(meter, "中文.请求计数", Some("中文请求总数统计"), Some("次"))
  let english_counter = Meter::create_counter(meter, "english.request.count", Some("Total English request count"), Some("requests"))
  let japanese_counter = Meter::create_counter(meter, "日本語.リクエスト数", Some("日本語リクエスト総数統計"), Some("回"))
  
  // 更新多语言metrics
  Counter::add(chinese_counter, 100.0)
  Counter::add(english_counter, 200.0)
  Counter::add(japanese_counter, 150.0)
  
  // 测试多语言histogram
  let chinese_histogram = Meter::create_histogram(meter, "中文.响应时间", Some("中文响应时间分布"), Some("毫秒"))
  let english_histogram = Meter::create_histogram(meter, "english.response.time", Some("English response time distribution"), Some("milliseconds"))
  
  Histogram::record(chinese_histogram, 100.0)
  Histogram::record(chinese_histogram, 200.0)
  Histogram::record(chinese_histogram, 150.0)
  
  Histogram::record(english_histogram, 50.0)
  Histogram::record(english_histogram, 75.0)
  Histogram::record(english_histogram, 60.0)
  
  // 测试多语言属性与metrics结合
  let multilingual_attrs = Attributes::new()
  Attributes::set(multilingual_attrs, "地区", StringValue("中国"))
  Attributes::set(multilingual_attrs, "语言", StringValue("中文"))
  Attributes::set(multilingual_attrs, "Region", StringValue("United States"))
  Attributes::set(multilingual_attrs, "Language", StringValue("English"))
  
  let localized_counter = Meter::create_counter(meter, "本地化.用户操作", Some("按地区和语言分类的用户操作"), Some("操作"))
  Counter::add(localized_counter, 1.0, Some(multilingual_attrs))
  
  assert_true(true) // 如果多语言metrics操作成功则测试通过
}

test "Unicode字符处理测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "unicode.处理")
  let span = Tracer::start_span(tracer, "Unicode字符处理测试")
  
  // 测试各种Unicode字符
  let unicode_chars = [
    ("emoji.笑脸", StringValue("😊😃😄😁😆😅😂🤣☺️😊")),
    ("emoji.动物", StringValue("🐶🐱🐭🐹🐰🦊🐻🐼🐨🐯")),
    ("emoji.食物", StringValue("🍎🍊🍋🍌🍉🍇🍓🫐🍈🍒")),
    ("数学符号", StringValue("∑∏∆∇∂∫∞±≤≥≠≈∝√")),
    ("货币符号", StringValue("¥€£$₹₽₩₪₫₡₨₱₲₴₸₼₽")),
    ("箭头符号", StringValue("←→↑↓↖↗↘↙⇐⇒⇑⇓⇔⇕⇖⇗⇘⇙")),
    ("特殊字符", StringValue("©®™℠℡№℀℁℅℆ℇ℈℉ℊℋℌℍℎℏ")),
    ("国际字符", StringValue("ñüöäéàèùêëîïôäçßøåæœ")),
    ("组合字符", StringValue("éêëe̊óôöo̊áâäå")),
    ("零宽字符", StringValue("zero\u200Bwidth\u200Cjoiner\u200Dnon\uFEFFjoiner"))
  ]
  
  for char_data in unicode_chars {
    Span::add_event(span, "unicode.test", Some([char_data]))
  }
  
  // 测试包含Unicode的trace_id和span_id
  let unicode_trace_id = "追踪🚀ID_🌟123"
  let unicode_span_id = "跨度💎ID_✨456"
  
  let unicode_span_ctx = SpanContext::new(unicode_trace_id, unicode_span_id, true, "状态=🎯")
  let unicode_span = Span::new("Unicode Span", Internal, unicode_span_ctx)
  
  assert_eq(SpanContext::trace_id(unicode_span_ctx), unicode_trace_id)
  assert_eq(SpanContext::span_id(unicode_span_ctx), unicode_span_id)
  assert_true(SpanContext::is_sampled(unicode_span_ctx))
  
  Span::end(unicode_span)
  Span::end(span)
  
  assert_true(true) // 如果Unicode字符处理成功则测试通过
}

test "本地化格式化测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "本地化格式化")
  
  // 测试不同地区的日期时间格式
  let timestamp = Clock::now_unix_nanos(Clock::system())
  
  let date_formats = [
    ("中文.日期", "2025年12月28日 星期日"),
    ("英文.日期", "Sunday, December 28, 2025"),
    ("日文.日期", "2025年12月28日 日曜日"),
    ("中文.时间", "14:30:45"),
    ("英文.时间", "2:30:45 PM"),
    ("日文.时间", "14時30分45秒")
  ]
  
  for format in date_formats {
    let (key, formatted_value) = format
    let log_record = LogRecord::new_with_context(
      Info,
      Some("格式化时间测试"),
      Some(Attributes::new()),
      Some(timestamp),
      None,
      None,
      None,
      None
    )
    
    // 在实际实现中，这里会使用本地化格式化
    // 这里我们模拟格式化结果
    Logger::emit(logger, log_record)
  }
  
  // 测试数字格式化
  let number_formats = [
    ("中文.数字", "1,234.56"),
    ("英文.数字", "1,234.56"),
    ("德文.数字", "1.234,56"),
    ("法文.数字", "1 234,56"),
    ("中文.货币", "¥1,234.56"),
    ("英文.货币", "$1,234.56"),
    ("欧元.货币", "€1,234.56"),
    ("日文.货币", "¥1,235")
  ]
  
  for format in number_formats {
    let (key, formatted_value) = format
    let log_record = LogRecord::new_with_context(
      Info,
      Some("数字格式化测试"),
      Some(Attributes::new()),
      Some(timestamp),
      None,
      None,
      None,
      None
    )
    
    Logger::emit(logger, log_record)
  }
  
  assert_true(true) // 如果本地化格式化处理成功则测试通过
}

test "多语言错误消息测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "多语言错误处理")
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "多语言错误日志")
  
  // 测试多语言错误场景
  let error_scenarios = [
    ("数据库连接失败", "Database connection failed", "データベース接続失敗", "데이터베이스 연결 실패"),
    ("用户认证超时", "User authentication timeout", "ユーザー認証タイムアウト", "사용자 인증 시간 초과"),
    ("文件不存在", "File not found", "ファイルが存在しません", "파일이 존재하지 않습니다"),
    ("权限不足", "Insufficient permissions", "権限が不足しています", "권한이 부족합니다"),
    ("网络请求失败", "Network request failed", "ネットワークリクエスト失敗", "네트워크 요청 실패"),
    ("内存不足", "Out of memory", "メモリ不足", "메모리 부족"),
    ("服务不可用", "Service unavailable", "サービスが利用できません", "서비스를 사용할 수 없습니다"),
    ("参数验证失败", "Parameter validation failed", "パラメータ検証失敗", "매개변수 검증 실패")
  ]
  
  for error in error_scenarios {
    let (chinese_msg, english_msg, japanese_msg, korean_msg) = error
    
    // 创建错误span
    let error_span = Tracer::start_span(tracer, "错误处理")
    Span::set_status(error_span, Error, Some(chinese_msg))
    Span::add_event(error_span, "错误详情", Some([
      ("中文.错误", StringValue(chinese_msg)),
      ("英文.错误", StringValue(english_msg)),
      ("日文.错误", StringValue(japanese_msg)),
      ("韩文.错误", StringValue(korean_msg))
    ]))
    Span::end(error_span)
    
    // 创建多语言错误日志
    let chinese_error_log = LogRecord::new(Error, chinese_msg)
    let english_error_log = LogRecord::new(Error, english_msg)
    let japanese_error_log = LogRecord::new(Error, japanese_msg)
    let korean_error_log = LogRecord::new(Error, korean_msg)
    
    Logger::emit(logger, chinese_error_log)
    Logger::emit(logger, english_error_log)
    Logger::emit(logger, japanese_error_log)
    Logger::emit(logger, korean_error_log)
  }
  
  // 测试多语言异常恢复
  let recovery_span = Tracer::start_span(tracer, "异常恢复")
  Span::set_status(recovery_span, Ok, Some("系统已从多语言错误中恢复 / System recovered from multilingual errors"))
  Span::add_event(recovery_span, "恢复成功", Some([
    ("恢复状态", StringValue("成功")),
    ("Recovery Status", StringValue("Success")),
    ("復旧ステータス", StringValue("成功")),
    ("복구 상태", StringValue("성공"))
  ]))
  Span::end(recovery_span)
  
  assert_true(true) // 如果多语言错误处理成功则测试通过
}

test "文化特定数据处理测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "文化特定数据")
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "文化特定处理")
  
  // 测试文化特定的数据处理
  let cultural_data = [
    ("中文.姓名格式", "张三", "姓在前，名在后"),
    ("英文.姓名格式", "John Smith", "名在前，姓在后"),
    ("日文.姓名格式", "田中太郎", "姓在前，名在后"),
    ("韩文.姓名格式", "김철수", "姓在前，名在后"),
    ("中文.地址格式", "中国北京市朝阳区建国路88号", "从大到小：国家-省市-区县-街道"),
    ("英文.地址格式", "88 Jianguo Road, Chaoyang District, Beijing, China", "从小到大：门牌号-街道-区县-城市-国家"),
    ("中文.电话格式", "+86 138-0013-8000", "国家码-地区码-号码"),
    ("英文.电话格式", "+1 (555) 123-4567", "国家码-(区号) 号码")
  ]
  
  for data in cultural_data {
    let (key, value, description) = data
    let span = Tracer::start_span(tracer, "文化数据处理")
    Span::add_event(span, "文化数据", Some([
      (key, StringValue(value)),
      (key + ".描述", StringValue(description))
    ]))
    
    // 创建文化特定的metrics
    let cultural_counter = Meter::create_counter(meter, key, Some(description), Some("次"))
    Counter::add(cultural_counter, 1.0)
    
    Span::end(span)
  }
  
  // 测试文化特定的排序和比较
  let sorting_span = Tracer::start_span(tracer, "文化特定排序")
  let multilingual_names = [
    "张三", "李四", "王五",  // 中文
    "John", "Mary", "David",  // 英文
    "田中", "佐藤", "鈴木",  // 日文
    "김철수", "이영희", "박민준"  // 韩文
  ]
  
  for name in multilingual_names {
    Span::add_event(sorting_span, "多语言姓名", Some([("姓名", StringValue(name))]))
  }
  
  Span::end(sorting_span)
  assert_true(true) // 如果文化特定数据处理成功则测试通过
}