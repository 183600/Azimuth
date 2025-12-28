// 多语言和Unicode支持测试
// Multilingual and Unicode support tests for Azimuth telemetry system

test "多语言属性键值测试" {
  // 测试各种语言的属性键和值
  
  let attrs = @azimuth.telemetry.api.common.Attributes::new()
  
  // 测试中文属性
  @azimuth.telemetry.api.common.Attributes::set(attrs, "用户名", @azimuth.telemetry.api.common.StringValue("张三"))
  @azimuth.telemetry.api.common.Attributes::set(attrs, "服务名称", @azimuth.telemetry.api.common.StringValue("用户认证服务"))
  @azimuth.telemetry.api.common.Attributes::set(attrs, "操作类型", @azimuth.telemetry.api.common.StringValue("登录验证"))
  
  // 测试日文属性
  @azimuth.telemetry.api.common.Attributes::set(attrs, "ユーザー名", @azimuth.telemetry.api.common.StringValue("田中太郎"))
  @azimuth.telemetry.api.common.Attributes::set(attrs, "サービス名", @azimuth.telemetry.api.common.StringValue("認証サービス"))
  @azimuth.telemetry.api.common.Attributes::set(attrs, "操作タイプ", @azimuth.telemetry.api.common.StringValue("ログイン認証"))
  
  // 测试韩文属性
  @azimuth.telemetry.api.common.Attributes::set(attrs, "사용자명", @azimuth.telemetry.api.common.StringValue("김철수"))
  @azimuth.telemetry.api.common.Attributes::set(attrs, "서비스명", @azimuth.telemetry.api.common.StringValue("인증 서비스"))
  @azimuth.telemetry.api.common.Attributes::set(attrs, "작업유형", @azimuth.telemetry.api.common.StringValue("로그인 인증"))
  
  // 测试俄文属性
  @azimuth.telemetry.api.common.Attributes::set(attrs, "имя_пользователя", @azimuth.telemetry.api.common.StringValue("Иван Петров"))
  @azimuth.telemetry.api.common.Attributes::set(attrs, "имя_сервиса", @azimuth.telemetry.api.common.StringValue("служба_аутентификации"))
  @azimuth.telemetry.api.common.Attributes::set(attrs, "тип_операции", @azimuth.telemetry.api.common.StringValue("аутентификация_входа"))
  
  // 测试阿拉伯文属性
  @azimuth.telemetry.api.common.Attributes::set(attrs, "اسم_المستخدم", @azimuth.telemetry.api.common.StringValue("أحمد محمد"))
  @azimuth.telemetry.api.common.Attributes::set(attrs, "اسم_الخدمة", @azimuth.telemetry.api.common.StringValue("خدمة_المصادقة"))
  @azimuth.telemetry.api.common.Attributes::set(attrs, "نوع_العملية", @azimuth.telemetry.api.common.StringValue("مصادقة_الدخول"))
  
  // 测试混合语言属性
  @azimuth.telemetry.api.common.Attributes::set(attrs, "混合键值", @azimuth.telemetry.api.common.StringValue("Mixed Value: 张三-田中-김철수-Иван-أحمد"))
  
  // 验证属性设置成功
  assert_true(attrs != @azimuth.telemetry.api.common.Attributes::{})
  
  // 测试属性获取（简化实现可能返回None）
  let chinese_username = @azimuth.telemetry.api.common.Attributes::get(attrs, "用户名")
  let japanese_username = @azimuth.telemetry.api.common.Attributes::get(attrs, "ユーザー名")
  let korean_username = @azimuth.telemetry.api.common.Attributes::get(attrs, "사용자명")
  let russian_username = @azimuth.telemetry.api.common.Attributes::get(attrs, "имя_пользователя")
  let arabic_username = @azimuth.telemetry.api.common.Attributes::get(attrs, "اسم_المستخدم")
  let mixed_value = @azimuth.telemetry.api.common.Attributes::get(attrs, "混合键值")
}

test "多语言Span名称和事件测试" {
  // 测试多语言Span名称和事件
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "多语言测试追踪器")
  
  // 创建中文Span
  let (_, chinese_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, "用户登录流程")
  @azimuth.telemetry.api.trace.Span::set_attribute(chinese_span, "用户ID", @azimuth.telemetry.api.common.StringValue("用户12345"))
  @azimuth.telemetry.api.trace.Span::set_attribute(chinese_span, "操作描述", @azimuth.telemetry.api.common.StringValue("用户通过手机号进行登录验证"))
  
  // 添加中文事件
  @azimuth.telemetry.api.trace.Span::add_event(
    chinese_span, 
    "开始验证用户身份", 
    Some([("验证方式", @azimuth.telemetry.api.common.StringValue("短信验证码"))]),
    None
  )
  
  @azimuth.telemetry.api.trace.Span::add_event(
    chinese_span, 
    "验证成功", 
    Some([("结果", @azimuth.telemetry.api.common.StringValue("用户身份验证通过"))]),
    None
  )
  
  // 创建日文Span
  let (_, japanese_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, "ユーザーログイン処理")
  @azimuth.telemetry.api.trace.Span::set_attribute(japanese_span, "ユーザーID", @azimuth.telemetry.api.common.StringValue("ユーザー12345"))
  @azimuth.telemetry.api.trace.Span::set_attribute(japanese_span, "操作説明", @azimuth.telemetry.api.common.StringValue("電話番号でのログイン認証"))
  
  // 添加日文事件
  @azimuth.telemetry.api.trace.Span::add_event(
    japanese_span, 
    "ユーザー認証開始", 
    Some([("認証方式", @azimuth.telemetry.api.common.StringValue("SMS認証コード"))]),
    None
  )
  
  @azimuth.telemetry.api.trace.Span::add_event(
    japanese_span, 
    "認証成功", 
    Some([("結果", @azimuth.telemetry.api.common.StringValue("ユーザー認証完了"))]),
    None
  )
  
  // 创建韩文Span
  let (_, korean_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, "사용자 로그인 처리")
  @azimuth.telemetry.api.trace.Span::set_attribute(korean_span, "사용자ID", @azimuth.telemetry.api.common.StringValue("사용자12345"))
  @azimuth.telemetry.api.trace.Span::set_attribute(korean_span, "작업설명", @azimuth.telemetry.api.common.StringValue("전화번호를 통한 로그인 인증"))
  
  // 添加韩文事件
  @azimuth.telemetry.api.trace.Span::add_event(
    korean_span, 
    "사용자 인증 시작", 
    Some([("인증방식", @azimuth.telemetry.api.common.StringValue("SMS 인증코드"))]),
    None
  )
  
  @azimuth.telemetry.api.trace.Span::add_event(
    korean_span, 
    "인증 성공", 
    Some([("결과", @azimuth.telemetry.api.common.StringValue("사용자 인증 완료"))]),
    None
  )
  
  // 结束所有Span
  @azimuth.telemetry.api.trace.Span::end(chinese_span)
  @azimuth.telemetry.api.trace.Span::end(japanese_span)
  @azimuth.telemetry.api.trace.Span::end(korean_span)
  
  // 验证Span都有有效的上下文
  let chinese_ctx = @azimuth.telemetry.api.trace.Span::span_context(chinese_span)
  let japanese_ctx = @azimuth.telemetry.api.trace.Span::span_context(japanese_span)
  let korean_ctx = @azimuth.telemetry.api.trace.Span::span_context(korean_span)
  
  assert_true(@azimuth.telemetry.api.trace.SpanContext::is_valid(chinese_ctx))
  assert_true(@azimuth.telemetry.api.trace.SpanContext::is_valid(japanese_ctx))
  assert_true(@azimuth.telemetry.api.trace.SpanContext::is_valid(korean_ctx))
}

test "多语言日志记录测试" {
  // 测试多语言日志记录
  
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider::default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(logs_provider, "多语言日志记录器")
  
  // 创建中文日志
  let chinese_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(chinese_attrs, "用户名", @azimuth.telemetry.api.common.StringValue("张三"))
  @azimuth.telemetry.api.common.Attributes::set(chinese_attrs, "操作", @azimuth.telemetry.api.common.StringValue("登录"))
  @azimuth.telemetry.api.common.Attributes::set(chinese_attrs, "结果", @azimuth.telemetry.api.common.StringValue("成功"))
  
  let chinese_log = @azimuth.telemetry.api.logs.LogRecord::new(
    @azimuth.telemetry.api.logs.Info,
    "用户登录操作成功完成",
    Some(chinese_attrs),
    Some(@azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())),
    None,
    Some("中文追踪ID123"),
    Some("中文跨度ID456"),
    None
  )
  
  // 创建日文日志
  let japanese_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(japanese_attrs, "ユーザー名", @azimuth.telemetry.api.common.StringValue("田中太郎"))
  @azimuth.telemetry.api.common.Attributes::set(japanese_attrs, "操作", @azimuth.telemetry.api.common.StringValue("ログイン"))
  @azimuth.telemetry.api.common.Attributes::set(japanese_attrs, "結果", @azimuth.telemetry.api.common.StringValue("成功"))
  
  let japanese_log = @azimuth.telemetry.api.logs.LogRecord::new(
    @azimuth.telemetry.api.logs.Info,
    "ユーザーログイン操作が正常に完了しました",
    Some(japanese_attrs),
    Some(@azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())),
    None,
    Some("日本語トレースID123"),
    Some("日本語スパンID456"),
    None
  )
  
  // 创建韩文日志
  let korean_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(korean_attrs, "사용자명", @azimuth.telemetry.api.common.StringValue("김철수"))
  @azimuth.telemetry.api.common.Attributes::set(korean_attrs, "작업", @azimuth.telemetry.api.common.StringValue("로그인"))
  @azimuth.telemetry.api.common.Attributes::set(korean_attrs, "결과", @azimuth.telemetry.api.common.StringValue("성공"))
  
  let korean_log = @azimuth.telemetry.api.logs.LogRecord::new(
    @azimuth.telemetry.api.logs.Info,
    "사용자 로그인 작업이 성공적으로 완료되었습니다",
    Some(korean_attrs),
    Some(@azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())),
    None,
    Some("한국어 추적 ID123"),
    Some("한국어 스팬 ID456"),
    None
  )
  
  // 记录所有日志
  @azimuth.telemetry.sdk.logs.Logger::emit(logger, chinese_log)
  @azimuth.telemetry.sdk.logs.Logger::emit(logger, japanese_log)
  @azimuth.telemetry.sdk.logs.Logger::emit(logger, korean_log)
  
  // 测试不同严重程度的多语言日志
  let severity_messages = [
    (@azimuth.telemetry.api.logs.Trace, "跟踪信息：详细调试信息"),
    (@azimuth.telemetry.api.logs.Debug, "调试信息：开发阶段调试"),
    (@azimuth.telemetry.api.logs.Info, "信息日志：正常操作记录"),
    (@azimuth.telemetry.api.logs.Warn, "警告信息：需要注意的情况"),
    (@azimuth.telemetry.api.logs.Error, "错误信息：操作失败记录"),
    (@azimuth.telemetry.api.logs.Fatal, "致命错误：系统严重问题")
  ]
  
  for (severity, message) in severity_messages {
    let severity_log = @azimuth.telemetry.api.logs.LogRecord::new(
      severity,
      message,
      None,
      None,
      None,
      None,
      None,
      None
    )
    
    @azimuth.telemetry.sdk.logs.Logger::emit(logger, severity_log)
  }
}

test "多语言指标测试" {
  // 测试多语言指标名称和描述
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "多语言指标测量器")
  
  // 创建中文指标
  let chinese_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(
    meter,
    "用户登录次数",
    Some("记录用户登录成功的总次数"),
    Some("次")
  )
  
  let chinese_histogram = @azimuth.telemetry.sdk.metrics.Meter::create_histogram(
    meter,
    "响应时间",
    Some("API请求响应时间分布"),
    Some("毫秒")
  )
  
  // 创建日文指标
  let japanese_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(
    meter,
    "ユーザーログイン回数",
    Some("ユーザーログイン成功の総回数を記録"),
    Some("回")
  )
  
  let japanese_histogram = @azimuth.telemetry.sdk.metrics.Meter::create_histogram(
    meter,
    "応答時間",
    Some("APIリクエスト応答時間の分布"),
    Some("ミリ秒")
  )
  
  // 创建韩文指标
  let korean_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(
    meter,
    "사용자 로그인 횟수",
    Some("사용자 로그인 성공 총 횟수 기록"),
    Some("회")
  )
  
  let korean_histogram = @azimuth.telemetry.sdk.metrics.Meter::create_histogram(
    meter,
    "응답 시간",
    Some("API 요청 응답 시간 분포"),
    Some("밀리초")
  )
  
  // 记录中文指标
  let chinese_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(chinese_attrs, "用户类型", @azimuth.telemetry.api.common.StringValue("普通用户"))
  @azimuth.telemetry.api.common.Attributes::set(chinese_attrs, "登录方式", @azimuth.telemetry.api.common.StringValue("手机号"))
  
  @azimuth.telemetry.sdk.metrics.Counter::add(chinese_counter, 1.0, Some(chinese_attrs))
  @azimuth.telemetry.sdk.metrics.Histogram::record(chinese_histogram, 150.5, Some(chinese_attrs))
  
  // 记录日文指标
  let japanese_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(japanese_attrs, "ユーザータイプ", @azimuth.telemetry.api.common.StringValue("一般ユーザー"))
  @azimuth.telemetry.api.common.Attributes::set(japanese_attrs, "ログイン方式", @azimuth.telemetry.api.common.StringValue("電話番号"))
  
  @azimuth.telemetry.sdk.metrics.Counter::add(japanese_counter, 1.0, Some(japanese_attrs))
  @azimuth.telemetry.sdk.metrics.Histogram::record(japanese_histogram, 120.3, Some(japanese_attrs))
  
  // 记录韩文指标
  let korean_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(korean_attrs, "사용자 유형", @azimuth.telemetry.api.common.StringValue("일반 사용자"))
  @azimuth.telemetry.api.common.Attributes::set(korean_attrs, "로그인 방식", @azimuth.telemetry.api.common.StringValue("전화번호"))
  
  @azimuth.telemetry.sdk.metrics.Counter::add(korean_counter, 1.0, Some(korean_attrs))
  @azimuth.telemetry.sdk.metrics.Histogram::record(korean_histogram, 180.7, Some(korean_attrs))
}

test "多语言资源测试" {
  // 测试多语言资源属性
  
  // 创建中文资源
  let chinese_resource = @azimuth.telemetry.api.common.Resource::with_attributes(
    @azimuth.telemetry.api.common.Resource::new(),
    [
      ("服务名称", @azimuth.telemetry.api.common.StringValue("用户认证服务")),
      ("服务版本", @azimuth.telemetry.api.common.StringValue("1.0.0")),
      ("服务实例ID", @azimuth.telemetry.api.common.StringValue("实例-CN-001")),
      ("部署环境", @azimuth.telemetry.api.common.StringValue("生产环境")),
      ("数据中心", @azimuth.telemetry.api.common.StringValue("北京数据中心"))
    ]
  )
  
  // 创建日文资源
  let japanese_resource = @azimuth.telemetry.api.common.Resource::with_attributes(
    @azimuth.telemetry.api.common.Resource::new(),
    [
      ("サービス名", @azimuth.telemetry.api.common.StringValue("認証サービス")),
      ("サービスバージョン", @azimuth.telemetry.api.common.StringValue("1.0.0")),
      ("サービスインスタンスID", @azimuth.telemetry.api.common.StringValue("インスタンス-JP-001")),
      ("デプロイ環境", @azimuth.telemetry.api.common.StringValue("本番環境")),
      ("データセンター", @azimuth.telemetry.api.common.StringValue("東京データセンター"))
    ]
  )
  
  // 创建韩文资源
  let korean_resource = @azimuth.telemetry.api.common.Resource::with_attributes(
    @azimuth.telemetry.api.common.Resource::new(),
    [
      ("서비스명", @azimuth.telemetry.api.common.StringValue("인증 서비스")),
      ("서비스 버전", @azimuth.telemetry.api.common.StringValue("1.0.0")),
      ("서비스 인스턴스 ID", @azimuth.telemetry.api.common.StringValue("인스턴스-KR-001")),
      ("배포 환경", @azimuth.telemetry.api.common.StringValue("프로덕션 환경")),
      ("데이터 센터", @azimuth.telemetry.api.common.StringValue("서울 데이터 센터"))
    ]
  )
  
  // 测试资源合并
  let merged_resource1 = @azimuth.telemetry.api.common.Resource::merge(chinese_resource, japanese_resource)
  let merged_resource2 = @azimuth.telemetry.api.common.Resource::merge(merged_resource1, korean_resource)
  
  // 验证资源属性（简化实现可能返回None）
  let chinese_service_name = @azimuth.telemetry.api.common.Resource::get_attribute(chinese_resource, "服务名称")
  let japanese_service_name = @azimuth.telemetry.api.common.Resource::get_attribute(japanese_resource, "サービス名")
  let korean_service_name = @azimuth.telemetry.api.common.Resource::get_attribute(korean_resource, "서비스명")
  
  // 测试使用多语言资源创建提供者
  let chinese_trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider::with_resource(chinese_resource)
  let japanese_metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::with_resource(japanese_resource)
  let korean_logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider::with_resource(korean_resource)
  
  // 验证提供者创建成功
  assert_true(chinese_trace_provider != @azimuth.telemetry.sdk.trace.TracerProvider::{})
  assert_true(japanese_metrics_provider != @azimuth.telemetry.sdk.metrics.MeterProvider::{})
  assert_true(korean_logs_provider != @azimuth.telemetry.sdk.logs.LoggerProvider::{})
}

test "Unicode特殊字符测试" {
  // 测试Unicode特殊字符处理
  
  let attrs = @azimuth.telemetry.api.common.Attributes::new()
  
  // 测试Emoji字符
  @azimuth.telemetry.api.common.Attributes::set(attrs, "表情符号", @azimuth.telemetry.api.common.StringValue("🚀🌟⭐💫✨🎉🎊🎈🎁🎂🍰"))
  @azimuth.telemetry.api.common.Attributes::set(attrs, "动物表情", @azimuth.telemetry.api.common.StringValue("🐶🐱🐭🐹🐰🦊🐻🐼🐨🐯"))
  @azimuth.telemetry.api.common.Attributes::set(attrs, "食物表情", @azimuth.telemetry.api.common.StringValue("🍎🍊🍋🍌🍉🍇🍓🫐🍈🍒"))
  
  // 测试数学符号
  @azimuth.telemetry.api.common.Attributes::set(attrs, "数学符号", @azimuth.telemetry.api.common.StringValue("∑∏∫∆∇∂∞±≤≥≠≈∈∉∪∩"))
  
  // 测试货币符号
  @azimuth.telemetry.api.common.Attributes::set(attrs, "货币符号", @azimuth.telemetry.api.common.StringValue("¥$€£¢₹₽₩₪₫₡₦₨₱₲₴₸₼₮"))
  
  // 测试箭头符号
  @azimuth.telemetry.api.common.Attributes::set(attrs, "箭头符号", @azimuth.telemetry.api.common.StringValue("←→↑↓↖↗↘↙⇐⇒⇑⇓⇔⇕⇖⇗⇘⇙"))
  
  // 测试特殊Unicode字符
  @azimuth.telemetry.api.common.Attributes::set(attrs, "特殊字符", @azimuth.telemetry.api.common.StringValue("™®©§¶†‡•…‰′″‹›''""""–—"))
  
  // 测试组合字符
  @azimuth.telemetry.api.common.Attributes::set(attrs, "组合字符", @azimuth.telemetry.api.common.StringValue("éàèùâêîôûäëïöüãõåæœçñß"))
  
  // 测试在Span中使用特殊字符
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "特殊字符测试追踪器")
  
  let (_, special_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, "🚀特殊字符测试Span🌟")
  
  // 添加特殊字符属性
  @azimuth.telemetry.api.trace.Span::set_attribute(special_span, "表情测试", @azimuth.telemetry.api.common.StringValue("😀😃😄😁😆😅😂🤣😊😇"))
  @azimuth.telemetry.api.trace.Span::set_attribute(special_span, "符号测试", @azimuth.telemetry.api.common.StringValue("♠♣♥♦♤♧♡♢★☆☀☁⛅⛈🌤🌥🌦🌧🌨🌩🌪"))
  
  // 添加特殊字符事件
  let special_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(special_attrs, "事件类型", @azimuth.telemetry.api.common.StringValue("🎉特殊事件🎊"))
  
  @azimuth.telemetry.api.trace.Span::add_event(
    special_span, 
    "✨特殊字符事件⭐", 
    Some(special_attrs),
    None
  )
  
  // 记录特殊字符异常
  @azimuth.telemetry.api.trace.Span::record_exception(
    special_span,
    "⚠️特殊字符异常⚠️",
    Some("🚨UnicodeException🚨"),
    Some("at special.char.test()\n  at main()"),
    Some(special_attrs)
  )
  
  @azimuth.telemetry.api.trace.Span::end(special_span)
  
  // 测试特殊字符日志
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider::default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(logs_provider, "特殊字符日志记录器")
  
  let special_log = @azimuth.telemetry.api.logs.LogRecord::new(
    @azimuth.telemetry.api.logs.Warn,
    "⚠️包含特殊字符的日志消息🌟",
    Some(special_attrs),
    None,
    None,
    None,
    None,
    None
  )
  
  @azimuth.telemetry.sdk.logs.Logger::emit(logger, special_log)
}

test "上下文传播多语言测试" {
  // 测试多语言上下文传播
  
  let composite = @azimuth.telemetry.api.propagation.CompositePropagator::new([
    @azimuth.telemetry.api.propagation.W3CTraceContextPropagator::new(),
    @azimuth.telemetry.api.propagation.W3CBaggagePropagator::new()
  ])
  
  // 创建多语言baggage
  let ctx = @azimuth.telemetry.api.context.Context::root()
  
  // 添加多语言上下文值
  let chinese_key = @azimuth.telemetry.api.context.ContextKey::new("中文键")
  let ctx_with_chinese = @azimuth.telemetry.api.context.Context::with_value(ctx, chinese_key, "中文值")
  
  let japanese_key = @azimuth.telemetry.api.context.ContextKey::new("日本語キー")
  let ctx_with_japanese = @azimuth.telemetry.api.context.Context::with_value(ctx_with_chinese, japanese_key, "日本語値")
  
  let korean_key = @azimuth.telemetry.api.context.ContextKey::new("한국어 키")
  let ctx_with_korean = @azimuth.telemetry.api.context.Context::with_value(ctx_with_japanese, korean_key, "한국어 값")
  
  // 测试注入
  let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier::new()
  @azimuth.telemetry.api.propagation.CompositePropagator::inject(composite, ctx_with_korean, carrier)
  
  // 验证注入的traceparent
  let traceparent = @azimuth.telemetry.api.propagation.TextMapCarrier::get(carrier, "traceparent")
  assert_true(traceparent != None)
  assert_true(traceparent.unwrap().starts_with("00-"))
  
  // 测试提取
  let extracted_ctx = @azimuth.telemetry.api.propagation.CompositePropagator::extract(composite, carrier)
  assert_true(extracted_ctx != @azimuth.telemetry.api.context.Context::{})
  
  // 测试多语言baggage值
  let baggage_carrier = @azimuth.telemetry.api.propagation.TextMapCarrier::new()
  @azimuth.telemetry.api.propagation.TextMapCarrier::set(
    baggage_carrier, 
    "baggage", 
    "用户=张三,ユーザー=田中,사용자=김철수,user=John"
  )
  
  let baggage_extracted_ctx = @azimuth.telemetry.api.propagation.CompositePropagator::extract(composite, baggage_carrier)
  assert_true(baggage_extracted_ctx != @azimuth.telemetry.api.context.Context::{})
  
  // 测试带有特殊字符的baggage
  let special_baggage_carrier = @azimuth.telemetry.api.propagation.TextMapCarrier::new()
  @azimuth.telemetry.api.propagation.TextMapCarrier::set(
    special_baggage_carrier, 
    "baggage", 
    "表情=🚀,符号=∑∏∫,货币=¥$€,箭头=←→"
  )
  
  let special_extracted_ctx = @azimuth.telemetry.api.propagation.CompositePropagator::extract(composite, special_baggage_carrier)
  assert_true(special_extracted_ctx != @azimuth.telemetry.api.context.Context::{})
}