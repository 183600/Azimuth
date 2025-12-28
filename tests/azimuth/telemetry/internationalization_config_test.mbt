// 国际化配置测试
// Internationalization configuration tests

test "多语言属性值测试" {
  // 测试多语言属性值的处理
  
  let attrs = Attributes::new()
  
  // 中文属性
  Attributes::set(attrs, "服务名称", StringValue("遥测服务"))
  Attributes::set(attrs, "服务版本", StringValue("1.0.0"))
  Attributes::set(attrs, "环境", StringValue("生产环境"))
  Attributes::set(attrs, "描述", StringValue("这是一个用于测试的遥测服务"))
  
  // 验证中文属性
  let service_name = Attributes::get(attrs, "服务名称")
  let service_version = Attributes::get(attrs, "服务版本")
  let environment = Attributes::get(attrs, "环境")
  let description = Attributes::get(attrs, "描述")
  
  // 英文属性
  Attributes::set(attrs, "service.name", StringValue("telemetry-service"))
  Attributes::set(attrs, "service.version", StringValue("1.0.0"))
  Attributes::set(attrs, "environment", StringValue("production"))
  Attributes::set(attrs, "description", StringValue("This is a telemetry service for testing"))
  
  // 验证英文属性
  let en_service_name = Attributes::get(attrs, "service.name")
  let en_service_version = Attributes::get(attrs, "service.version")
  let en_environment = Attributes::get(attrs, "environment")
  let en_description = Attributes::get(attrs, "description")
  
  // 俄文属性
  Attributes::set(attrs, "имя.сервиса", StringValue("служба.телеметрии"))
  Attributes::set(attrs, "версия.сервиса", StringValue("1.0.0"))
  Attributes::set(attrs, "окружение", StringValue("производство"))
  Attributes::set(attrs, "описание", StringValue("Это служба телеметрии для тестирования"))
  
  // 法文属性
  Attributes::set(attrs, "nom.service", StringValue("service.telemetrie"))
  Attributes::set(attrs, "version.service", StringValue("1.0.0"))
  Attributes::set(attrs, "environnement", StringValue("production"))
  Attributes::set(attrs, "description", StringValue("Ceci est un service de télémétrie pour les tests"))
  
  // 德文属性
  Attributes::set(attrs, "dienst.name", StringValue("telemetrie-dienst"))
  Attributes::set(attrs, "dienst.version", StringValue("1.0.0"))
  Attributes::set(attrs, "umgebung", StringValue("produktion"))
  Attributes::set(attrs, "beschreibung", StringValue("Dies ist ein Telemetriedienst zum Testen"))
  
  // 日文属性
  Attributes::set(attrs, "サービス名", StringValue("テレメトリーサービス"))
  Attributes::set(attrs, "サービスバージョン", StringValue("1.0.0"))
  Attributes::set(attrs, "環境", StringValue("本番環境"))
  Attributes::set(attrs, "説明", StringValue("これはテスト用のテレメトリサービスです"))
  
  // 韩文属性
  Attributes::set(attrs, "서비스.이름", StringValue("원격측정.서비스"))
  Attributes::set(attrs, "서비스.버전", StringValue("1.0.0"))
  Attributes::set(attrs, "환경", StringValue("프로덕션"))
  Attributes::set(attrs, "설명", StringValue("이것은 테스트용 원격 측정 서비스입니다"))
  
  // 阿拉伯文属性
  Attributes::set(attrs, "اسم.الخدمة", StringValue("خدمة.قياس_بعد"))
  Attributes::set(attrs, "إصدار.الخدمة", StringValue("1.0.0"))
  Attributes::set(attrs, "البيئة", StringValue("الإنتاج"))
  Attributes::set(attrs, "الوصف", StringValue("هذه خدمة قياس بعد للاختبار"))
}

test "多语言日志消息测试" {
  // 测试多语言日志消息的处理
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "i18n-logger")
  
  // 中文日志
  let chinese_log = LogRecord::new(Info, "操作开始执行")
  let chinese_error = LogRecord::new(Error, "发生严重错误，需要立即处理")
  let chinese_warning = LogRecord::new(Warn, "警告：系统资源使用率过高")
  
  Logger::emit(logger, chinese_log)
  Logger::emit(logger, chinese_error)
  Logger::emit(logger, chinese_warning)
  
  // 英文日志
  let english_log = LogRecord::new(Info, "Operation started")
  let english_error = LogRecord::new(Error, "Critical error occurred, immediate attention required")
  let english_warning = LogRecord::new(Warn, "Warning: High system resource usage")
  
  Logger::emit(logger, english_log)
  Logger::emit(logger, english_error)
  Logger::emit(logger, english_warning)
  
  // 俄文日志
  let russian_log = LogRecord::new(Info, "Операция начата")
  let russian_error = LogRecord::new(Error, "Произошла критическая ошибка, требуется немедленное внимание")
  let russian_warning = LogRecord::new(Warn, "Предупреждение: Высокое использование системных ресурсов")
  
  Logger::emit(logger, russian_log)
  Logger::emit(logger, russian_error)
  Logger::emit(logger, russian_warning)
  
  // 法文日志
  let french_log = LogRecord::new(Info, "Opération démarrée")
  let french_error = LogRecord::new(Error, "Erreur critique survenue, attention immédiate requise")
  let french_warning = LogRecord::new(Warn, "Avertissement: Utilisation élevée des ressources système")
  
  Logger::emit(logger, french_log)
  Logger::emit(logger, french_error)
  Logger::emit(logger, french_warning)
  
  // 德文日志
  let german_log = LogRecord::new(Info, "Operation gestartet")
  let german_error = LogRecord::new(Error, "Kritischer Fehler aufgetreten, sofortige Aufmerksamkeit erforderlich")
  let german_warning = LogRecord::new(Warn, "Warnung: Hohe Systemressourcennutzung")
  
  Logger::emit(logger, german_log)
  Logger::emit(logger, german_error)
  Logger::emit(logger, german_warning)
  
  // 日文日志
  let japanese_log = LogRecord::new(Info, "操作が開始されました")
  let japanese_error = LogRecord::new(Error, "重大なエラーが発生しました。直ちに対処が必要です")
  let japanese_warning = LogRecord::new(Warn, "警告：システムリソース使用率が高いです")
  
  Logger::emit(logger, japanese_log)
  Logger::emit(logger, japanese_error)
  Logger::emit(logger, japanese_warning)
  
  // 韩文日志
  let korean_log = LogRecord::new(Info, "작업이 시작되었습니다")
  let korean_error = LogRecord::new(Error, "치명적인 오류가 발생했습니다. 즉각적인 주의가 필요합니다")
  let korean_warning = LogRecord::new(Warn, "경고: 시스템 리소스 사용량이 높습니다")
  
  Logger::emit(logger, korean_log)
  Logger::emit(logger, korean_error)
  Logger::emit(logger, korean_warning)
  
  // 混合语言日志
  let mixed_log = LogRecord::new(Info, "Mixed language message: 中文 English Русский Français")
  Logger::emit(logger, mixed_log)
}

test "多语言Span事件测试" {
  // 测试多语言Span事件的处理
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "i18n-tracer")
  
  // 中文Span事件
  let chinese_span = Tracer::start_span(tracer, "中文操作")
  Span::add_event(chinese_span, "开始处理", Some([
    ("操作类型", StringValue("数据处理")),
    ("用户ID", StringValue("用户123"))
  ]))
  Span::add_event(chinese_span, "处理完成", Some([
    ("处理记录数", IntValue(1000)),
    ("耗时", IntValue(5000))
  ]))
  Span::end(chinese_span)
  
  // 英文Span事件
  let english_span = Tracer::start_span(tracer, "English Operation")
  Span::add_event(english_span, "Processing Started", Some([
    ("operation.type", StringValue("data_processing")),
    ("user.id", StringValue("user123"))
  ]))
  Span::add_event(english_span, "Processing Completed", Some([
    ("records.processed", IntValue(1000)),
    ("duration.ms", IntValue(5000))
  ]))
  Span::end(english_span)
  
  // 俄文Span事件
  let russian_span = Tracer::start_span(tracer, "Русская Операция")
  Span::add_event(russian_span, "Обработка Начата", Some([
    ("тип.операции", StringValue("обработка.данных")),
    ("id.пользователя", StringValue("пользователь123"))
  ]))
  Span::add_event(russian_span, "Обработка Завершена", Some([
    ("записей.обработано", IntValue(1000)),
    ("длительность.мс", IntValue(5000))
  ]))
  Span::end(russian_span)
  
  // 法文Span事件
  let french_span = Tracer::start_span(tracer, "Opération Française")
  Span::add_event(french_span, "Traitement Démarré", Some([
    ("type.opération", StringValue("traitement_données")),
    ("id.utilisateur", StringValue("utilisateur123"))
  ]))
  Span::add_event(french_span, "Traitement Terminé", Some([
    ("enregistements.traits", IntValue(1000)),
    ("duree.ms", IntValue(5000))
  ]))
  Span::end(french_span)
  
  // 混合语言Span事件
  let mixed_span = Tracer::start_span(tracer, "Mixed Language Operation")
  Span::add_event(mixed_span, "混合事件", Some([
    ("中文属性", StringValue("中文值")),
    ("English Attribute", StringValue("English Value")),
    ("Русский атрибут", StringValue("Русское значение")),
    ("Attribut français", StringValue("Valeur française"))
  ]))
  Span::end(mixed_span)
}

test "多语言资源配置测试" {
  // 测试多语言资源配置
  
  // 中文资源配置
  let chinese_resource = Resource::new()
  let chinese_attrs = [
    ("服务名称", StringValue("遥测服务")),
    ("服务版本", StringValue("1.0.0")),
    ("服务实例", StringValue("实例-001")),
    ("主机名称", StringValue("生产服务器-01")),
    ("操作系统", StringValue("Linux")),
    ("环境", StringValue("生产环境")),
    ("数据中心", StringValue("北京数据中心")),
    ("团队", StringValue("平台团队"))
  ]
  
  let chinese_resource_with_attrs = Resource::with_attributes(chinese_resource, chinese_attrs)
  
  // 英文资源配置
  let english_resource = Resource::new()
  let english_attrs = [
    ("service.name", StringValue("telemetry-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-001")),
    ("host.name", StringValue("production-server-01")),
    ("os.type", StringValue("Linux")),
    ("environment", StringValue("production")),
    ("datacenter", StringValue("beijing-datacenter")),
    ("team", StringValue("platform-team"))
  ]
  
  let english_resource_with_attrs = Resource::with_attributes(english_resource, english_attrs)
  
  // 俄文资源配置
  let russian_resource = Resource::new()
  let russian_attrs = [
    ("имя.сервиса", StringValue("служба.телеметрии")),
    ("версия.сервиса", StringValue("1.0.0")),
    ("id.экземпляра.сервиса", StringValue("экземпляр-001")),
    ("имя.хоста", StringValue("производственный.сервер-01")),
    ("тип.ос", StringValue("Linux")),
    ("окружение", StringValue("производство")),
    ("центр.обработки.данных", StringValue("пекинский.цод")),
    ("команда", StringValue("команда.платформы"))
  ]
  
  let russian_resource_with_attrs = Resource::with_attributes(russian_resource, russian_attrs)
  
  // 测试资源合并
  let merged_resource = Resource::merge(chinese_resource_with_attrs, english_resource_with_attrs)
  
  // 验证资源属性
  let chinese_service_name = Resource::get_attribute(chinese_resource_with_attrs, "服务名称")
  let english_service_name = Resource::get_attribute(english_resource_with_attrs, "service.name")
  let russian_service_name = Resource::get_attribute(russian_resource_with_attrs, "имя.сервиса")
}

test "多语言指标名称和描述测试" {
  // 测试多语言指标名称和描述
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "i18n-meter")
  
  // 中文指标
  let chinese_counter = Meter::create_counter(meter, "请求总数", Some("接收到的HTTP请求总数"), Some("个"))
  let chinese_histogram = Meter::create_histogram(meter, "响应时间", Some("HTTP请求响应时间分布"), Some("毫秒"))
  let chinese_gauge = Meter::create_histogram(meter, "内存使用率", Some("当前内存使用百分比"), Some("百分比"))
  
  // 英文指标
  let english_counter = Meter::create_counter(meter, "http.requests.total", Some("Total number of HTTP requests received"), Some("count"))
  let english_histogram = Meter::create_histogram(meter, "http.response.time", Some("HTTP request response time distribution"), Some("ms"))
  let english_gauge = Meter::create_histogram(meter, "memory.usage", Some("Current memory usage percentage"), Some("percent"))
  
  // 俄文指标
  let russian_counter = Meter::create_counter(meter, "всего.запросов", Some("Общее количество полученных HTTP-запросов"), Some("штука"))
  let russian_histogram = Meter::create_histogram(meter, "время.ответа", Some("Распределение времени ответа HTTP-запроса"), Some("мс"))
  let russian_gauge = Meter::create_histogram(meter, "использование.памяти", Some("Текущий процент использования памяти"), Some("процент"))
  
  // 法文指标
  let french_counter = Meter::create_counter(meter, "total.requetes.http", Some("Nombre total de requêtes HTTP reçues"), Some("compte"))
  let french_histogram = Meter::create_histogram(meter, "temps.reponse.http", Some("Distribution du temps de réponse des requêtes HTTP"), Some("ms"))
  let french_gauge = Meter::create_histogram(meter, "utilisation.memoire", Some("Pourcentage d'utilisation mémoire actuelle"), Some("pourcentage"))
  
  // 记录指标值
  Counter::add(chinese_counter, 100.0)
  Histogram::record(chinese_histogram, 150.5)
  
  Counter::add(english_counter, 200.0)
  Histogram::record(english_histogram, 120.3)
  
  Counter::add(russian_counter, 150.0)
  Histogram::record(russian_histogram, 180.7)
  
  Counter::add(french_counter, 120.0)
  Histogram::record(french_histogram, 95.2)
  
  // 验证指标属性
  assert_eq(chinese_counter.name, "请求总数")
  assert_eq(english_counter.name, "http.requests.total")
  assert_eq(russian_counter.name, "всего.запросов")
  assert_eq(french_counter.name, "total.requetes.http")
  
  assert_eq(chinese_counter.description, Some("接收到的HTTP请求总数"))
  assert_eq(english_counter.description, Some("Total number of HTTP requests received"))
  assert_eq(russian_counter.description, Some("Общее количество полученных HTTP-запросов"))
  assert_eq(french_counter.description, Some("Nombre total de requêtes HTTP reçues"))
}

test "多语言上下文传播测试" {
  // 测试多语言上下文的传播
  
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // 创建包含多语言信息的上下文
  let root_ctx = Context::root()
  
  // 中文上下文值
  let chinese_key = ContextKey::new("用户信息")
  let ctx_with_chinese = Context::with_value(root_ctx, chinese_key, "用户张三，来自北京")
  
  // 英文上下文值
  let english_key = ContextKey::new("user.info")
  let ctx_with_english = Context::with_value(ctx_with_chinese, english_key, "User John Doe from New York")
  
  // 俄文上下文值
  let russian_key = ContextKey::new("информация.о.пользователе")
  let ctx_with_russian = Context::with_value(ctx_with_english, russian_key, "Пользователь Иван Иванов из Москвы")
  
  // 注入到Carrier
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx_with_russian, carrier)
  
  // 添加多语言自定义头部
  TextMapCarrier::set(carrier, "X-User-Name-CN", "张三")
  TextMapCarrier::set(carrier, "X-User-Name-EN", "John Doe")
  TextMapCarrier::set(carrier, "X-User-Name-RU", "Иван Иванов")
  
  TextMapCarrier::set(carrier, "X-Location-CN", "北京")
  TextMapCarrier::set(carrier, "X-Location-EN", "New York")
  TextMapCarrier::set(carrier, "X-Location-RU", "Москва")
  
  // 从Carrier提取
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // 验证提取的多语言信息
  let chinese_user = Context::get(ctx_with_russian, chinese_key)
  let english_user = Context::get(ctx_with_russian, english_key)
  let russian_user = Context::get(ctx_with_russian, russian_key)
  
  // 验证Carrier中的多语言头部
  let chinese_name = TextMapCarrier::get(carrier, "X-User-Name-CN")
  let english_name = TextMapCarrier::get(carrier, "X-User-Name-EN")
  let russian_name = TextMapCarrier::get(carrier, "X-User-Name-RU")
  
  let chinese_location = TextMapCarrier::get(carrier, "X-Location-CN")
  let english_location = TextMapCarrier::get(carrier, "X-Location-EN")
  let russian_location = TextMapCarrier::get(carrier, "X-Location-RU")
}

test "本地化错误消息测试" {
  // 测试本地化错误消息
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "i18n-error-logger")
  
  // 中文错误消息
  let chinese_errors = [
    "数据库连接失败",
    "网络超时错误",
    "权限验证失败",
    "资源不足错误",
    "配置文件格式错误"
  ]
  
  for i = 0; i < chinese_errors.length(); i = i + 1 {
    let error_log = LogRecord::new(Error, chinese_errors[i])
    Logger::emit(logger, error_log)
  }
  
  // 英文错误消息
  let english_errors = [
    "Database connection failed",
    "Network timeout error",
    "Authentication failed",
    "Insufficient resources error",
    "Configuration file format error"
  ]
  
  for i = 0; i < english_errors.length(); i = i + 1 {
    let error_log = LogRecord::new(Error, english_errors[i])
    Logger::emit(logger, error_log)
  }
  
  // 俄文错误消息
  let russian_errors = [
    "Не удалось подключиться к базе данных",
    "Ошибка тайм-аута сети",
    "Ошибка аутентификации",
    "Ошибка недостаточных ресурсов",
    "Ошибка формата файла конфигурации"
  ]
  
  for i = 0; i < russian_errors.length(); i = i + 1 {
    let error_log = LogRecord::new(Error, russian_errors[i])
    Logger::emit(logger, error_log)
  }
  
  // 法文错误消息
  let french_errors = [
    "Échec de la connexion à la base de données",
    "Erreur de timeout réseau",
    "Échec de l'authentification",
    "Erreur de ressources insuffisantes",
    "Erreur de format de fichier de configuration"
  ]
  
  for i = 0; i < french_errors.length(); i = i + 1 {
    let error_log = LogRecord::new(Error, french_errors[i])
    Logger::emit(logger, error_log)
  }
  
  // 创建带本地化属性的Span错误状态
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "i18n-error-tracer")
  
  let error_span = Tracer::start_span(tracer, "error-operation")
  
  // 添加多语言错误事件
  Span::add_event(error_span, "错误发生", Some([
    ("错误代码", StringValue("DB_CONN_001")),
    ("错误消息", StringValue("数据库连接失败")),
    ("错误详情", StringValue("无法连接到主数据库服务器"))
  ]))
  
  Span::add_event(error_span, "Error Occurred", Some([
    ("error.code", StringValue("DB_CONN_001")),
    ("error.message", StringValue("Database connection failed")),
    ("error.details", StringValue("Unable to connect to primary database server"))
  ]))
  
  Span::set_status(error_span, Error, Some("操作失败"))
  Span::end(error_span)
}