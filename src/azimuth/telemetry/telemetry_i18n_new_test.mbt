// Azimuth Telemetry - Internationalization Support Test
// 测试国际化功能

use azimuth.telemetry.api.common
use azimuth.telemetry.api.context
use azimuth.telemetry.api.trace
use azimuth.telemetry.api.metrics
use azimuth.telemetry.api.logs

// 国际化测试数据
pub struct I18nTestData {
  language : String
  region : String
  script : String?
  display_name : String
  sample_text : String
  special_characters : String
  date_format : String
  number_format : String
}

pub fn I18nTestData::new(
  language : String,
  region : String,
  script : String?,
  display_name : String,
  sample_text : String,
  special_characters : String,
  date_format : String,
  number_format : String
) -> I18nTestData {
  I18nTestData::{
    language,
    region,
    script,
    display_name,
    sample_text,
    special_characters,
    date_format,
    number_format
  }
}

// 获取测试用的国际化数据
pub fn get_i18n_test_data() -> Array[I18nTestData] {
  [
    I18nTestData::new(
      "zh",
      "CN",
      Some("Hans"),
      "简体中文 (中国)",
      "这是一个测试消息，用于验证中文支持。包含特殊字符：！@#￥%……&*（）——+-={}【】|；'\"：""？/《》<>",
      "中文特殊字符：！@#￥%……&*（）——+-={}【】|；'\"：""？/《》<>",
      "YYYY年MM月DD日 HH:mm:ss",
      "1,234,567.89"
    ),
    I18nTestData::new(
      "zh",
      "TW",
      Some("Hant"),
      "繁體中文 (台灣)",
      "這是一個測試訊息，用於驗證繁體中文支援。包含特殊字元：！@#￥%……&*（）——+-={}【】|；'\"：""？/《》<>",
      "繁體中文特殊字元：！@#￥%……&*（）——+-={}【】|；'\"：""？/《》<>",
      "YYYY年MM月DD日 HH:mm:ss",
      "1,234,567.89"
    ),
    I18nTestData::new(
      "ja",
      "JP",
      Some("Jpan"),
      "日本語 (日本)",
      "これは日本語サポートを検証するためのテストメッセージです。特殊文字：！＠＃＄％＾＆＊（）－＋＝｛｝【】｜；'\"：""？／＜＞",
      "日本語特殊文字：！＠＃＄％＾＆＊（）－＋＝｛｝【】｜；'\"：""？／＜＞",
      "YYYY年MM月DD日 HH:mm:ss",
      "1,234,567.89"
    ),
    I18nTestData::new(
      "ko",
      "KR",
      Some("Kore"),
      "한국어 (한국)",
      "이것은 한국어 지원을 검증하기 위한 테스트 메시지입니다. 특수 문자：！@#￥%……&*（）——+-={}【】|；'\"：""？/《》<>",
      "한국어 특수 문자：！@#￥%……&*（）——+-={}【】|；'\"：""？/《》<>",
      "YYYY년 MM월 DD일 HH:mm:ss",
      "1,234,567.89"
    ),
    I18nTestData::new(
      "ar",
      "SA",
      Some("Arab"),
      "العربية (المملكة العربية السعودية)",
      "هذه رسالة اختبار للتحقق من دعم اللغة العربية. أحرف خاصة：！@#￥%……&*（）——+-={}【】|；'\"：""？/《》<>",
      "أحرف عربية خاصة：！@#￥%……&*（）——+-={}【】|；'\"：""？/《》<>",
      "YYYY/MM/DD HH:mm:ss",
      "1,234,567.89"
    ),
    I18nTestData::new(
      "he",
      "IL",
      Some("Hebr"),
      "עברית (ישראל)",
      "זוהי הודעת בדיקה לאימות תמיכה בעברית. תווים מיוחדים：！@#￥%……&*（）——+-={}【】|；'\"：""？/《》<>",
      "תווים עבריים מיוחדים：！@#￥%……&*（）——+-={}【】|；'\"：""？/《》<>",
      "DD/MM/YYYY HH:mm:ss",
      "1,234,567.89"
    ),
    I18nTestData::new(
      "ru",
      "RU",
      Some("Cyrl"),
      "Русский (Россия)",
      "Это тестовое сообщение для проверки поддержки русского языка. Специальные символы：！@#￥%……&*（）——+-={}【】|；'\"：""？/《》<>",
      "Русские специальные символы：！@#￥%……&*（）——+-={}【】|；'\"：""？/《》<>",
      "DD.MM.YYYY HH:mm:ss",
      "1 234 567,89"
    ),
    I18nTestData::new(
      "de",
      "DE",
      Some("Latn"),
      "Deutsch (Deutschland)",
      "Dies ist eine Testnachricht zur Überprüfung der deutschen Unterstützung. Sonderzeichen：!@#$%^&*()_-+={}[]|;':\"?,/<>",
      "Deutsche Sonderzeichen：!@#$%^&*()_-+={}[]|;':\"?,/<>ßäöüÄÖÜ",
      "DD.MM.YYYY HH:mm:ss",
      "1.234.567,89"
    ),
    I18nTestData::new(
      "fr",
      "FR",
      Some("Latn"),
      "Français (France)",
      "Ceci est un message de test pour vérifier le support français. Caractères spéciaux：!@#$%^&*()_-+={}[]|;':\"?,/<>",
      "Caractères spéciaux français：!@#$%^&*()_-+={}[]|;':\"?,/<>àâäéèêëïîôöùûüÿñç",
      "DD/MM/YYYY HH:mm:ss",
      "1 234 567,89"
    ),
    I18nTestData::new(
      "es",
      "ES",
      Some("Latn"),
      "Español (España)",
      "Este es un mensaje de prueba para verificar el soporte español. Caracteres especiales：!@#$%^&*()_-+={}[]|;':\"?,/<>",
      "Caracteres especiales españoles：!@#$%^&*()_-+={}[]|;':\"?,/<>ñáéíóúüÑÁÉÍÓÚÜ¿¡",
      "DD/MM/YYYY HH:mm:ss",
      "1.234.567,89"
    ),
    I18nTestData::new(
      "en",
      "US",
      Some("Latn"),
      "English (United States)",
      "This is a test message to verify English support. Special characters：!@#$%^&*()_-+={}[]|;':\"?,/<>",
      "English special characters：!@#$%^&*()_-+={}[]|;':\"?,/<>",
      "MM/DD/YYYY HH:mm:ss",
      "1,234,567.89"
    ),
    I18nTestData::new(
      "th",
      "TH",
      Some("Thai"),
      "ไทย (ประเทศไทย)",
      "นี่คือข้อความทดสอบเพื่อตรวจสอบการสนับสนุนภาษาไทย อักขระพิเศษ：!@#$%^&*()_-+={}[]|;':\"?,/<>",
      "อักขระพิเศษไทย：!@#$%^&*()_-+={}[]|;':\"?,/<>กขฃคฅฆงจฉชซฌญฎฏฐฑฒณดตถทธนบปผฝพฟภมยรลวศษสหฬอฮ",
      "DD/MM/YYYY HH:mm:ss",
      "1,234,567.89"
    )
  ]
}

test "trace_i18n_support" {
  // 测试Trace API的国际化支持
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("i18n-test-tracer", "1.0.0")
  let ctx = context::Context::new()
  let test_data = get_i18n_test_data()
  
  // 测试每种语言的Span名称和属性
  let mut i = 0
  while i < test_data.length() {
    let data = test_data[i]
    
    // 创建包含国际化内容的Span
    let (_, span) = tracer.start_span(
      ctx,
      data.sample_text,
      trace::Internal,
      [
        ("language", common::AttributeValue::string(data.language)),
        ("region", common::AttributeValue::string(data.region)),
        ("script", common::AttributeValue::string(data.script.unwrap_or(""))),
        ("display.name", common::AttributeValue::string(data.display_name)),
        ("special.chars", common::AttributeValue::string(data.special_characters)),
        ("date.format", common::AttributeValue::string(data.date_format)),
        ("number.format", common::AttributeValue::string(data.number_format))
      ],
      1234567890L
    )
    
    // 验证国际化内容正确保存
    @assert(span.name == data.sample_text)
    @assert(span.attributes.length() == 7)
    
    // 验证每个属性值都正确保存
    let mut j = 0
    while j < span.attributes.length() {
      let (key, value) = span.attributes[j]
      @assert(match value { common::StringValue(s) => s.length() > 0 _ => false })
      j = j + 1
    }
    
    i = i + 1
  }
}

test "metrics_i18n_support" {
  // 测试Metrics API的国际化支持
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("i18n-test-meter", "1.0.0")
  let test_data = get_i18n_test_data()
  
  // 测试每种语言的指标名称和属性
  let mut i = 0
  while i < test_data.length() {
    let data = test_data[i]
    
    // 创建包含国际化内容的指标
    let counter = meter.create_counter(
      data.sample_text + ".counter",
      "count",
      data.display_name + " counter"
    )
    
    let histogram = meter.create_histogram(
      data.sample_text + ".histogram",
      "ms",
      data.display_name + " histogram"
    )
    
    let up_down_counter = meter.create_up_down_counter(
      data.sample_text + ".up_down_counter",
      "count",
      data.display_name + " up-down counter"
    )
    
    let gauge = meter.create_gauge(
      data.sample_text + ".gauge",
      "value",
      data.display_name + " gauge"
    )
    
    // 记录包含国际化属性的指标
    let i18n_attributes = [
      ("language", common::AttributeValue::string(data.language)),
      ("region", common::AttributeValue::string(data.region)),
      ("script", common::AttributeValue::string(data.script.unwrap_or(""))),
      ("display.name", common::AttributeValue::string(data.display_name)),
      ("special.chars", common::AttributeValue::string(data.special_characters)),
      ("date.format", common::AttributeValue::string(data.date_format)),
      ("number.format", common::AttributeValue::string(data.number_format))
    ]
    
    counter.add(1L, i18n_attributes)
    histogram.record(100.5, i18n_attributes)
    up_down_counter.add(1L, i18n_attributes)
    gauge.record(50.25, i18n_attributes)
    
    // 验证国际化属性正确处理
    @assert(i18n_attributes.length() == 7)
    
    let mut j = 0
    while j < i18n_attributes.length() {
      let (key, value) = i18n_attributes[j]
      @assert(match value { common::StringValue(s) => s.length() > 0 _ => false })
      j = j + 1
    }
    
    i = i + 1
  }
}

test "logs_i18n_support" {
  // 测试Logs API的国际化支持
  
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("i18n-test-logger", "1.0.0")
  let test_data = get_i18n_test_data()
  
  // 测试每种语言的日志消息
  let mut i = 0
  while i < test_data.length() {
    let data = test_data[i]
    
    // 使用便捷方法记录国际化日志
    logger.debug(data.sample_text, [
      ("language", common::AttributeValue::string(data.language)),
      ("region", common::AttributeValue::string(data.region)),
      ("log.level", common::AttributeValue::string("debug"))
    ])
    
    logger.info(data.sample_text, [
      ("language", common::AttributeValue::string(data.language)),
      ("region", common::AttributeValue::string(data.region)),
      ("log.level", common::AttributeValue::string("info"))
    ])
    
    logger.warn(data.sample_text, [
      ("language", common::AttributeValue::string(data.language)),
      ("region", common::AttributeValue::string(data.region)),
      ("log.level", common::AttributeValue::string("warn"))
    ])
    
    logger.error(data.sample_text, [
      ("language", common::AttributeValue::string(data.language)),
      ("region", common::AttributeValue::string(data.region)),
      ("log.level", common::AttributeValue::string("error"))
    ])
    
    logger.fatal(data.sample_text, [
      ("language", common::AttributeValue::string(data.language)),
      ("region", common::AttributeValue::string(data.region)),
      ("log.level", common::AttributeValue::string("fatal"))
    ])
    
    // 创建包含国际化内容的LogRecord
    let log_record = logs::LogRecord::builder()
      .timestamp(1234567890L)
      .severity(logs::Info)
      .severity_text("INFO")
      .body(data.sample_text)
      .with_attribute("language", common::AttributeValue::string(data.language))
      .with_attribute("region", common::AttributeValue::string(data.region))
      .with_attribute("script", common::AttributeValue::string(data.script.unwrap_or("")))
      .with_attribute("display.name", common::AttributeValue::string(data.display_name))
      .with_attribute("special.chars", common::AttributeValue::string(data.special_characters))
      .with_attribute("date.format", common::AttributeValue::string(data.date_format))
      .with_attribute("number.format", common::AttributeValue::string(data.number_format))
      .build()
    
    logger.emit(log_record)
    
    i = i + 1
  }
}

test "attribute_i18n_support" {
  // 测试属性值的国际化支持
  
  let test_data = get_i18n_test_data()
  
  // 测试各种国际化字符串属性
  let mut i = 0
  while i < test_data.length() {
    let data = test_data[i]
    
    // 创建各种类型的国际化属性
    let string_attr = common::AttributeValue::string(data.sample_text)
    let display_name_attr = common::AttributeValue::string(data.display_name)
    let special_chars_attr = common::AttributeValue::string(data.special_characters)
    let date_format_attr = common::AttributeValue::string(data.date_format)
    let number_format_attr = common::AttributeValue::string(data.number_format)
    
    // 创建包含国际化内容的数组属性
    let string_array_attr = common::AttributeValue::array_string([
      data.sample_text,
      data.display_name,
      data.special_characters
    ])
    
    // 验证字符串属性
    @assert(match string_attr { common::StringValue(s) => s == data.sample_text _ => false })
    @assert(match display_name_attr { common::StringValue(s) => s == data.display_name _ => false })
    @assert(match special_chars_attr { common::StringValue(s) => s == data.special_characters _ => false })
    @assert(match date_format_attr { common::StringValue(s) => s == data.date_format _ => false })
    @assert(match number_format_attr { common::StringValue(s) => s == data.number_format _ => false })
    
    // 验证数组属性
    @assert(match string_array_attr { 
      common::ArrayStringValue(arr) => {
        arr.length() == 3 && 
        arr[0] == data.sample_text && 
        arr[1] == data.display_name && 
        arr[2] == data.special_characters
      } 
      _ => false 
    })
    
    i = i + 1
  }
}

test "resource_i18n_support" {
  // 测试资源的国际化支持
  
  let test_data = get_i18n_test_data()
  
  // 测试每种语言的资源配置
  let mut i = 0
  while i < test_data.length() {
    let data = test_data[i]
    
    // 创建包含国际化内容的资源
    let i18n_resource = common::Resource::{
      service_name: data.display_name + " service",
      service_version: Some("1.0.0"),
      telemetry_sdk_name: "azimuth",
      telemetry_sdk_version: "0.1.0",
      attributes: [
        ("language", common::AttributeValue::string(data.language)),
        ("region", common::AttributeValue::string(data.region)),
        ("script", common::AttributeValue::string(data.script.unwrap_or(""))),
        ("description", common::AttributeValue::string(data.sample_text)),
        ("special.chars", common::AttributeValue::string(data.special_characters)),
        ("date.format", common::AttributeValue::string(data.date_format)),
        ("number.format", common::AttributeValue::string(data.number_format))
      ]
    }
    
    // 验证国际化资源属性
    @assert(i18n_resource.service_name.contains(data.display_name))
    @assert(i18n_resource.attributes.length() == 6)
    
    let mut j = 0
    while j < i18n_resource.attributes.length() {
      let (key, value) = i18n_resource.attributes[j]
      @assert(match value { common::StringValue(s) => s.length() > 0 _ => false })
      j = j + 1
    }
    
    i = i + 1
  }
}

test "cross_api_i18n_consistency" {
  // 测试跨API的国际化一致性
  
  let test_data = get_i18n_test_data()
  
  // 测试每种语言在所有API中的一致性
  let mut i = 0
  while i < test_data.length() {
    let data = test_data[i]
    
    // 创建一致的国际化属性集
    let i18n_attributes = [
      ("language", common::AttributeValue::string(data.language)),
      ("region", common::AttributeValue::string(data.region)),
      ("script", common::AttributeValue::string(data.script.unwrap_or(""))),
      ("display.name", common::AttributeValue::string(data.display_name)),
      ("sample.text", common::AttributeValue::string(data.sample_text)),
      ("special.chars", common::AttributeValue::string(data.special_characters))
    ]
    
    // 在Trace中使用国际化属性
    let tracer_provider = trace::NoopTracerProvider::{}
    let tracer = tracer_provider.get_tracer("i18n-consistency-tracer", "1.0.0")
    let (_, span) = tracer.start_span(
      context::Context::new(),
      data.sample_text,
      trace::Internal,
      i18n_attributes,
      1234567890L
    )
    
    // 在Metrics中使用国际化属性
    let meter_provider = metrics::NoopMeterProvider::{}
    let meter = meter_provider.get_meter("i18n-consistency-meter", "1.0.0")
    let counter = meter.create_counter(data.display_name + ".counter", "count", "Consistency test counter")
    counter.add(1L, i18n_attributes)
    
    // 在Logs中使用国际化属性
    let logger_provider = logs::NoopLoggerProvider::{}
    let logger = logger_provider.get_logger("i18n-consistency-logger", "1.0.0")
    let log_record = logs::LogRecord::builder()
      .severity(logs::Info)
      .body(data.sample_text)
      .with_attribute("language", common::AttributeValue::string(data.language))
      .with_attribute("region", common::AttributeValue::string(data.region))
      .with_attribute("script", common::AttributeValue::string(data.script.unwrap_or("")))
      .with_attribute("display.name", common::AttributeValue::string(data.display_name))
      .with_attribute("sample.text", common::AttributeValue::string(data.sample_text))
      .with_attribute("special.chars", common::AttributeValue::string(data.special_characters))
      .build()
    
    logger.emit(log_record)
    
    // 验证跨API的一致性
    @assert(span.name == data.sample_text)
    @assert(span.attributes.length() == 6)
    
    let mut j = 0
    while j < i18n_attributes.length() {
      let (key, value) = i18n_attributes[j]
      @assert(match value { common::StringValue(s) => s.length() > 0 _ => false })
      j = j + 1
    }
    
    i = i + 1
  }
}

test "i18n_special_characters_handling" {
  // 测试特殊字符的处理
  
  let test_data = get_i18n_test_data()
  
  // 测试每种语言的特殊字符处理
  let mut i = 0
  while i < test_data.length() {
    let data = test_data[i]
    
    // 创建包含特殊字符的属性
    let special_char_attr = common::AttributeValue::string(data.special_characters)
    
    // 验证特殊字符正确保存
    @assert(match special_char_attr { 
      common::StringValue(s) => s == data.special_characters && s.length() > 0 
      _ => false 
    })
    
    // 测试特殊字符在Span中的处理
    let tracer_provider = trace::NoopTracerProvider::{}
    let tracer = tracer_provider.get_tracer("special-chars-tracer", "1.0.0")
    let (_, special_span) = tracer.start_span(
      context::Context::new(),
      "special-chars-test-" + data.language,
      trace::Internal,
      [("special.chars", common::AttributeValue::string(data.special_characters))],
      1234567890L
    )
    
    @assert(special_span.attributes.length() == 1)
    @assert(match special_span.attributes[0].1 { 
      common::StringValue(s) => s == data.special_characters 
      _ => false 
    })
    
    // 测试特殊字符在日志中的处理
    let logger_provider = logs::NoopLoggerProvider::{}
    let logger = logger_provider.get_logger("special-chars-logger", "1.0.0")
    logger.info(data.special_characters, [
      ("language", common::AttributeValue::string(data.language)),
      ("special.chars", common::AttributeValue::string(data.special_characters))
    ])
    
    i = i + 1
  }
}

test "i18n_unicode_support" {
  // 测试Unicode支持
  
  // 测试各种Unicode字符
  let unicode_test_cases = [
    ("Emoji", "🚀🌟💻📱🔧⚡"),
    ("Mathematical", "∑∏∫∆∇∂∞±≤≥≠≈∈∉⊂⊃∧∨¬"),
    ("Currency", "$€£¥₹₽₩₪₫₡₨₦₱₲₴₸₼₽"),
    ("Arrows", "←→↑↓↖↗↘↙⇐⇒⇑⇓⇔⇕⇖⇗⇘⇙"),
    ("Geometric", "■□▲△●○◆◇★☆♠♣♥♦"),
    ("Combining", "e\u0301a\u0300o\u0302u\u0308n\u0303"),
    ("Miscellaneous", "™®©§¶†‡•…‰‹›""''–—"),
    ("Braille", "⠁⠂⠃⠄⠅⠆⠇⠈⠉⠊⠋⠌⠍⠎⠏"),
    ("Box Drawing", "─│┌┐└┘├┤┬┴┼╭╮╯╰╱╲╳"),
    ("CJK Symbols", "〈〉《》「」『』【】〔〕〖〗")
  ]
  
  // 测试每个Unicode字符集
  let mut i = 0
  while i < unicode_test_cases.length() {
    let (name, chars) = unicode_test_cases[i]
    
    // 创建包含Unicode字符的属性
    let unicode_attr = common::AttributeValue::string(chars)
    
    // 验证Unicode字符正确保存
    @assert(match unicode_attr { 
      common::StringValue(s) => s == chars && s.length() > 0 
      _ => false 
    })
    
    // 在Span中使用Unicode字符
    let tracer_provider = trace::NoopTracerProvider::{}
    let tracer = tracer_provider.get_tracer("unicode-test-tracer", "1.0.0")
    let (_, unicode_span) = tracer.start_span(
      context::Context::new(),
      "unicode-test-" + name,
      trace::Internal,
      [
        ("unicode.name", common::AttributeValue::string(name)),
        ("unicode.chars", common::AttributeValue::string(chars))
      ],
      1234567890L
    )
    
    @assert(unicode_span.attributes.length() == 2)
    
    // 在日志中使用Unicode字符
    let logger_provider = logs::NoopLoggerProvider::{}
    let logger = logger_provider.get_logger("unicode-test-logger", "1.0.0")
    logger.info("Unicode test: " + name, [
      ("unicode.name", common::AttributeValue::string(name)),
      ("unicode.chars", common::AttributeValue::string(chars))
    ])
    
    // 创建包含Unicode字符的LogRecord
    let unicode_log = logs::LogRecord::builder()
      .severity(logs::Info)
      .body("Unicode " + name + " test: " + chars)
      .with_attribute("unicode.name", common::AttributeValue::string(name))
      .with_attribute("unicode.chars", common::AttributeValue::string(chars))
      .build()
    
    logger.emit(unicode_log)
    
    i = i + 1
  }
}