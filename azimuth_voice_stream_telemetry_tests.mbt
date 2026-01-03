// Azimuth 语音流遥测测试
// 测试语音流处理和实时通信的遥测功能

// 测试1: 语音通话质量遥测
test "语音通话质量遥测" {
  // 1. 创建语音通话质量指标
  let mos_score_metric = Gauge({
    name: "voice.call.quality.mos",
    description: Some("语音通话平均意见评分"),
    unit: Some("score")
  })
  
  let packet_loss_metric = Gauge({
    name: "voice.call.packet.loss",
    description: Some("语音通话丢包率"),
    unit: Some("percent")
  })
  
  let jitter_metric = Gauge({
    name: "voice.call.jitter",
    description: Some("语音抖动"),
    unit: Some("ms")
  })
  
  let latency_metric = Gauge({
    name: "voice.call.latency",
    description: Some("语音通话延迟"),
    unit: Some("ms")
  })
  
  let r_factor_metric = Gauge({
    name: "voice.call.r.factor",
    description: Some("语音通话R因子"),
    unit: Some("score")
  })
  
  // 2. 创建语音通话资源
  let call_resource = Resource::new([
    ("call.id", StringValue("call_20250103_001")),
    ("call.type", StringValue("video_conference")),
    ("participant.count", IntValue(4)),
    ("call.protocol", StringValue("WebRTC")),
    ("codec.used", StringValue("Opus")),
    ("call.initiator", StringValue("user_alice")),
    ("call.start.time", LongValue(1704297600000))
  ])
  
  // 3. 验证语音通话资源
  assert_eq(Resource::get_attribute(call_resource, "call.id"), Some(StringValue("call_20250103_001")))
  assert_eq(Resource::get_attribute(call_resource, "call.type"), Some(StringValue("video_conference")))
  assert_eq(Resource::get_attribute(call_resource, "participant.count"), Some(IntValue(4)))
  assert_eq(Resource::get_attribute(call_resource, "call.protocol"), Some(StringValue("WebRTC")))
  assert_eq(Resource::get_attribute(call_resource, "codec.used"), Some(StringValue("Opus")))
  
  // 4. 模拟语音通话质量监测过程
  let tracer = Tracer::new("voice_quality_tracer")
  let call_span = Tracer::start_span(tracer, "voice.call.quality.monitoring")
  
  // 添加通话配置
  Span::set_attributes(call_span, [
    ("monitoring.interval", IntValue(5)),
    ("quality.threshold.mos", DoubleValue(3.5)),
    ("quality.threshold.packet.loss", DoubleValue(3.0)),
    ("quality.threshold.jitter", DoubleValue(30.0)),
    ("quality.threshold.latency", DoubleValue(150.0))
  ])
  
  // 模拟不同时间的通话质量数据
  let quality_measurements = [
    (1704297600, 4.2, 0.5, 5.2, 45, 85),  // 00:00
    (1704297630, 4.0, 1.2, 8.5, 52, 82),  // 00:00:30
    (1704297660, 3.8, 2.1, 12.3, 68, 78), // 00:01:00
    (1704297690, 3.5, 3.5, 18.7, 95, 73), // 00:01:30
    (1704297720, 3.9, 1.8, 9.4, 58, 80),  // 00:02:00
    (1704297750, 4.1, 0.8, 6.1, 48, 84)   // 00:02:30
  ]
  
  for (timestamp, mos, packet_loss, jitter, latency, r_factor) in quality_measurements {
    Span::add_event(call_span, "voice.quality.measured", Some([
      ("measurement.timestamp", LongValue(timestamp)),
      ("mos.score", DoubleValue(mos)),
      ("packet.loss.percent", DoubleValue(packet_loss)),
      ("jitter.ms", DoubleValue(jitter)),
      ("latency.ms", IntValue(latency)),
      ("r.factor", IntValue(r_factor)),
      ("call.quality.category", StringValue(
        if mos >= 4.0 { "excellent" }
        else if mos >= 3.5 { "good" }
        else if mos >= 3.0 { "fair" }
        else { "poor" }
      ))
    ]))
  }
  
  // 添加网络质量变化事件
  Span::add_event(call_span, "network.quality.degraded", Some([
    ("event.timestamp", LongValue(1704297690)),
    ("degradation.type", StringValue("increased_latency")),
    ("latency.before", IntValue(68)),
    ("latency.after", IntValue(95)),
    ("impact.duration", IntValue(120)),
    ("adaptive.action", StringValue("bitrate_adjustment"))
  ]))
  
  // 添加编解码器切换事件
  Span::add_event(call_span, "codec.switch.initiated", Some([
    ("switch.timestamp", LongValue(1704297720)),
    ("codec.from", StringValue("Opus")),
    ("codec.to", StringValue("G.711")),
    ("switch.reason", StringValue("network_conditions")),
    ("switch.result", StringValue("successful"))
  ]))
  
  Span::end(call_span)
  
  // 5. 验证指标数据
  assert_eq(mos_score_metric.name, "voice.call.quality.mos")
  assert_eq(packet_loss_metric.name, "voice.call.packet.loss")
  assert_eq(jitter_metric.name, "voice.call.jitter")
  assert_eq(latency_metric.name, "voice.call.latency")
  assert_eq(r_factor_metric.name, "voice.call.r.factor")
}

// 测试2: 语音识别服务遥测
test "语音识别服务遥测" {
  // 1. 创建语音识别指标
  let recognition_accuracy_metric = Gauge({
    name: "voice.recognition.accuracy",
    description: Some("语音识别准确率"),
    unit: Some("percent")
  })
  
  let recognition_latency_metric = Histogram({
    name: "voice.recognition.latency",
    description: Some("语音识别延迟"),
    unit: Some("ms")
  })
  
  let word_error_rate_metric = Gauge({
    name: "voice.recognition.word.error.rate",
    description: Some("词错误率"),
    unit: Some("percent")
  })
  
  let language_detection_confidence_metric = Gauge({
    name: "voice.language.detection.confidence",
    description: Some("语言检测置信度"),
    unit: Some("score")
  })
  
  // 2. 创建语音识别服务资源
  let recognition_resource = Resource::new([
    ("service.id", StringValue("speech_recognition_003")),
    ("service.provider", StringValue("azimuth_ai")),
    ("model.version", StringValue("whisper_large_v3")),
    ("supported.languages", StringValue("en,zh,ja,es,fr")),
    ("service.region", StringValue("asia_east1")),
    ("api.version", StringValue("v2.1"))
  ])
  
  // 3. 验证语音识别服务资源
  assert_eq(Resource::get_attribute(recognition_resource, "service.id"), Some(StringValue("speech_recognition_003")))
  assert_eq(Resource::get_attribute(recognition_resource, "service.provider"), Some(StringValue("azimuth_ai")))
  assert_eq(Resource::get_attribute(recognition_resource, "model.version"), Some(StringValue("whisper_large_v3")))
  assert_eq(Resource::get_attribute(recognition_resource, "supported.languages"), Some(StringValue("en,zh,ja,es,fr")))
  
  // 4. 模拟语音识别过程
  let tracer = Tracer::new("speech_recognition_tracer")
  let recognition_span = Tracer::start_span(tracer, "voice.recognition.processing")
  
  // 添加识别配置
  Span::set_attributes(recognition_span, [
    ("audio.format", StringValue("PCM_16kHz_16bit")),
    ("recognition.mode", StringValue("streaming")),
    ("enhancement.enabled", BoolValue(true)),
    ("punctuation.enabled", BoolValue(true)),
    ("profanity.filter", BoolValue(false))
  ])
  
  // 模拟不同语音片段的识别结果
  let recognition_results = [
    (1704297600, "zh", "你好，今天天气不错", 0.92, 95.2, 4.8, 320),
    (1704297650, "en", "Hello, how are you today?", 0.89, 91.5, 8.5, 285),
    (1704297700, "ja", "こんにちは、元気ですか", 0.87, 89.3, 10.7, 410),
    (1704297750, "es", "Hola, ¿cómo estás hoy?", 0.91, 93.8, 6.2, 295),
    (1704297800, "fr", "Bonjour, comment allez-vous?", 0.88, 90.1, 9.9, 380)
  ]
  
  for (timestamp, language, transcript, confidence, accuracy, wer, latency) in recognition_results {
    Span::add_event(recognition_span, "speech.recognized", Some([
      ("recognition.timestamp", LongValue(timestamp)),
      ("detected.language", StringValue(language)),
      ("transcript.text", StringValue(transcript)),
      ("confidence.score", DoubleValue(confidence)),
      ("accuracy.percent", DoubleValue(accuracy)),
      ("word.error.rate", DoubleValue(wer)),
      ("processing.latency.ms", IntValue(latency)),
      ("recognition.quality", StringValue(
        if accuracy >= 95.0 { "excellent" }
        else if accuracy >= 90.0 { "good" }
        else if accuracy >= 85.0 { "fair" }
        else { "poor" }
      ))
    ]))
  }
  
  // 添加语言切换事件
  Span::add_event(recognition_span, "language.switch.detected", Some([
    ("switch.timestamp", LongValue(1704297650)),
    ("language.from", StringValue("zh")),
    ("language.to", StringValue("en")),
    ("switch.confidence", DoubleValue(0.94)),
    ("adaptation.time.ms", IntValue(150))
  ]))
  
  // 添加音频质量异常事件
  Span::add_event(recognition_span, "audio.quality.issue", Some([
    ("issue.timestamp", LongValue(1704297700)),
    ("issue.type", StringValue("background_noise")),
    ("noise.level", DoubleValue(0.65)),
    ("impact.on.accuracy", DoubleValue(5.9)),
    ("noise.suppression.applied", BoolValue(true))
  ]))
  
  Span::end(recognition_span)
  
  // 5. 验证指标数据
  assert_eq(recognition_accuracy_metric.name, "voice.recognition.accuracy")
  assert_eq(recognition_latency_metric.name, "voice.recognition.latency")
  assert_eq(word_error_rate_metric.name, "voice.recognition.word.error.rate")
  assert_eq(language_detection_confidence_metric.name, "voice.language.detection.confidence")
}

// 测试3: 实时语音翻译遥测
test "实时语音翻译遥测" {
  // 1. 创建语音翻译指标
  let translation_latency_metric = Histogram({
    name: "voice.translation.latency",
    description: Some("语音翻译延迟"),
    unit: Some("ms")
  })
  
  let translation_quality_metric = Gauge({
    name: "voice.translation.quality",
    description: Some("语音翻译质量评分"),
    unit: Some("score")
  })
  
  let bleu_score_metric = Gauge({
    name: "voice.translation.bleu.score",
    description: Some("BLEU评分"),
    unit: Some("score")
  })
  
  let language_pair_metric = Counter({
    name: "voice.translation.language.pair.requests",
    description: Some("语言对翻译请求次数"),
    unit: Some("requests")
  })
  
  // 2. 创建语音翻译服务资源
  let translation_resource = Resource::new([
    ("service.id", StringValue("realtime_translation_001")),
    ("translation.engine", StringValue("azimuth_neural_mt")),
    ("model.version", StringValue("transformer_v4.2")),
    ("supported.directions", IntValue(42)),
    ("streaming.enabled", BoolValue(true)),
    ("domain.specialization", StringValue("business_conference"))
  ])
  
  // 3. 验证语音翻译服务资源
  assert_eq(Resource::get_attribute(translation_resource, "service.id"), Some(StringValue("realtime_translation_001")))
  assert_eq(Resource::get_attribute(translation_resource, "translation.engine"), Some(StringValue("azimuth_neural_mt")))
  assert_eq(Resource::get_attribute(translation_resource, "model.version"), Some(StringValue("transformer_v4.2")))
  assert_eq(Resource::get_attribute(translation_resource, "supported.directions"), Some(IntValue(42)))
  
  // 4. 模拟实时语音翻译过程
  let tracer = Tracer::new("voice_translation_tracer")
  let translation_span = Tracer::start_span(tracer, "realtime.voice.translation")
  
  // 添加翻译配置
  Span::set_attributes(translation_span, [
    ("source.language", StringValue("zh")),
    ("target.language", StringValue("en")),
    ("translation.mode", StringValue("streaming")),
    ("latency.budget.ms", IntValue(500)),
    ("quality.threshold", DoubleValue(0.85))
  ])
  
  // 模拟不同语音片段的翻译结果
  let translation_results = [
    (1704297600, "我想讨论一下我们的合作计划", "I would like to discuss our cooperation plan", "zh-en", 0.88, 0.82, 420),
    (1704297650, "这个项目预计需要三个月完成", "This project is expected to be completed in three months", "zh-en", 0.91, 0.85, 380),
    (1704297700, "我们需要更多的市场调研数据", "We need more market research data", "zh-en", 0.86, 0.79, 450),
    (1704297750, "预算方面是否有调整空间？", "Is there room for budget adjustments?", "zh-en", 0.89, 0.83, 410),
    (1704297800, "下周二可以安排一次详细讨论", "A detailed discussion can be arranged next Tuesday", "zh-en", 0.92, 0.87, 390)
  ]
  
  for (timestamp, source_text, target_text, lang_pair, quality, bleu, latency) in translation_results {
    Span::add_event(translation_span, "voice.translated", Some([
      ("translation.timestamp", LongValue(timestamp)),
      ("source.text", StringValue(source_text)),
      ("target.text", StringValue(target_text)),
      ("language.pair", StringValue(lang_pair)),
      ("quality.score", DoubleValue(quality)),
      ("bleu.score", DoubleValue(bleu)),
      ("translation.latency.ms", IntValue(latency)),
      ("translation.rating", StringValue(
        if quality >= 0.90 { "excellent" }
        else if quality >= 0.85 { "good" }
        else if quality >= 0.80 { "acceptable" }
        else { "needs_improvement" }
      ))
    ]))
  }
  
  // 添加翻译模型切换事件
  Span::add_event(translation_span, "model.switch.initiated", Some([
    ("switch.timestamp", LongValue(1704297700)),
    ("model.from", StringValue("general_purpose")),
    ("model.to", StringValue("business_domain")),
    ("switch.reason", StringValue("context_detection")),
    ("switch.impact", StringValue("improved_accuracy")
  ]))
  
  // 添加翻译质量反馈事件
  Span::add_event(translation_span, "user.feedback.received", Some([
    ("feedback.timestamp", LongValue(1704297800)),
    ("user.rating", IntValue(4)),
    ("user.comment", StringValue("Translation is accurate and natural")),
    ("feedback.used.for.improvement", BoolValue(true))
  ]))
  
  Span::end(translation_span)
  
  // 5. 验证指标数据
  assert_eq(translation_latency_metric.name, "voice.translation.latency")
  assert_eq(translation_quality_metric.name, "voice.translation.quality")
  assert_eq(bleu_score_metric.name, "voice.translation.bleu.score")
  assert_eq(language_pair_metric.name, "voice.translation.language.pair.requests")
}

// 测试4: 语音合成服务遥测
test "语音合成服务遥测" {
  // 1. 创建语音合成指标
  let synthesis_latency_metric = Histogram({
    name: "voice.synthesis.latency",
    description: Some("语音合成延迟"),
    unit: Some("ms")
  })
  
  let naturalness_score_metric = Gauge({
    name: "voice.synthesis.naturalness.score",
    description: Some("语音自然度评分"),
    unit: Some("score")
  })
  
  let articulation_clarity_metric = Gauge({
    name: "voice.synthesis.articulation.clarity",
    description: Some("发音清晰度"),
    unit: Some("score")
  })
  
  let voice_characteristic_match_metric = Gauge({
    name: "voice.synthesis.characteristic.match",
    description: Some("语音特征匹配度"),
    unit: Some("percent")
  })
  
  // 2. 创建语音合成服务资源
  let synthesis_resource = Resource::new([
    ("service.id", StringValue("tts_service_007")),
    ("synthesis.engine", StringValue("azimuth_neural_voice")),
    ("model.version", StringValue("tacotron2_wavernn_v3")),
    ("supported.voices", IntValue(127)),
    ("audio.output.format", StringValue("PCM_24kHz_16bit")),
    ("realtime.capable", BoolValue(true))
  ])
  
  // 3. 验证语音合成服务资源
  assert_eq(Resource::get_attribute(synthesis_resource, "service.id"), Some(StringValue("tts_service_007")))
  assert_eq(Resource::get_attribute(synthesis_resource, "synthesis.engine"), Some(StringValue("azimuth_neural_voice")))
  assert_eq(Resource::get_attribute(synthesis_resource, "model.version"), Some(StringValue("tacotron2_wavernn_v3")))
  assert_eq(Resource::get_attribute(synthesis_resource, "supported.voices"), Some(IntValue(127)))
  
  // 4. 模拟语音合成过程
  let tracer = Tracer::new("voice_synthesis_tracer")
  let synthesis_span = Tracer::start_span(tracer, "text.to.speech.synthesis")
  
  // 添加合成配置
  Span::set_attributes(synthesis_span, [
    ("input.text.length", IntValue(156)),
    ("target.voice", StringValue("female_professional_en_us")),
    ("speaking.rate", DoubleValue(1.0)),
    ("pitch.adjustment", DoubleValue(0.0)),
    ("emotion.style", StringValue("neutral"))
  ])
  
  // 模拟不同文本的合成结果
  let synthesis_results = [
    (1704297600, "Welcome to our quarterly business review meeting", "en-US", 4.2, 4.5, 92, 850),
    (1704297700, "今天我们将讨论第四季度的业绩表现", "zh-CN", 4.0, 4.2, 88, 920),
    (1704297800, "Les résultats du trimestre dernier dépassent nos attentes", "fr-FR", 3.8, 4.0, 85, 980),
    (1704297900, "Los ingresos han aumentado un quince por ciento", "es-ES", 3.9, 4.1, 87, 940),
    (1704298000, "来年度の予算案について説明します", "ja-JP", 3.7, 3.9, 83, 1020)
  ]
  
  for (timestamp, text, language, naturalness, clarity, characteristic_match, latency) in synthesis_results {
    Span::add_event(synthesis_span, "speech.synthesized", Some([
      ("synthesis.timestamp", LongValue(timestamp)),
      ("input.text", StringValue(text)),
      ("synthesized.language", StringValue(language)),
      ("naturalness.score", DoubleValue(naturalness)),
      ("articulation.clarity", DoubleValue(clarity)),
      ("characteristic.match.percent", IntValue(characteristic_match)),
      ("synthesis.latency.ms", IntValue(latency)),
      ("audio.quality.rating", StringValue(
        if naturalness >= 4.0 && clarity >= 4.2 { "premium" }
        else if naturalness >= 3.5 && clarity >= 3.8 { "high" }
        else if naturalness >= 3.0 && clarity >= 3.5 { "standard" }
        else { "needs_improvement" }
      ))
    ]))
  }
  
  // 添加语音特征调整事件
  Span::add_event(synthesis_span, "voice.prosody.adjustment", Some([
    ("adjustment.timestamp", LongValue(1704297700)),
    ("adjustment.type", StringValue("speaking_rate")),
    ("original.value", DoubleValue(1.0)),
    ("adjusted.value", DoubleValue(0.95)),
    ("adjustment.reason", StringValue("content_type_presentation"))
  ]))
  
  // 添加情感风格切换事件
  Span::add_event(synthesis_span, "emotion.style.switch", Some([
    ("switch.timestamp", LongValue(1704297900)),
    ("style.from", StringValue("neutral")),
    ("style.to", StringValue("enthusiastic")),
    ("switch.context", StringValue("positive_achievement")),
    ("style.transfer.success", BoolValue(true))
  ]))
  
  Span::end(synthesis_span)
  
  // 5. 验证指标数据
  assert_eq(synthesis_latency_metric.name, "voice.synthesis.latency")
  assert_eq(naturalness_score_metric.name, "voice.synthesis.naturalness.score")
  assert_eq(articulation_clarity_metric.name, "voice.synthesis.articulation.clarity")
  assert_eq(voice_characteristic_match_metric.name, "voice.synthesis.characteristic.match")
}