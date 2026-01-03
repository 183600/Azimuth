// Azimuth 国际化支持测试用例
// 测试多语言、本地化和文化适应性功能

test "多语言错误消息本地化" {
  // 创建国际化资源管理器
  let i18n_manager = @azimuth.I18nManager {
    default_locale: "en-US",
    supported_locales: ["en-US", "zh-CN", "ja-JP", "fr-FR", "de-DE", "es-ES"],
    resource_bundles: @azimuth.Map.from_list([
      ("en-US", @azimuth.ResourceBundle {
        locale: "en-US",
        messages: @azimuth.Map.from_list([
          ("error.connection.timeout", "Connection timeout after {0}ms"),
          ("error.connection.refused", "Connection refused"),
          ("error.database.unavailable", "Database is unavailable"),
          ("error.validation.failed", "Validation failed: {0}"),
          ("error.authentication.failed", "Authentication failed"),
          ("error.authorization.denied", "Access denied"),
          ("error.resource.not_found", "Resource not found: {0}"),
          ("error.rate.limit.exceeded", "Rate limit exceeded"),
          ("error.internal.server", "Internal server error"),
          ("error.service.unavailable", "Service temporarily unavailable")
        ])
      }),
      ("zh-CN", @azimuth.ResourceBundle {
        locale: "zh-CN",
        messages: @azimuth.Map.from_list([
          ("error.connection.timeout", "连接超时，已等待{0}毫秒"),
          ("error.connection.refused", "连接被拒绝"),
          ("error.database.unavailable", "数据库不可用"),
          ("error.validation.failed", "验证失败：{0}"),
          ("error.authentication.failed", "身份验证失败"),
          ("error.authorization.denied", "访问被拒绝"),
          ("error.resource.not_found", "资源未找到：{0}"),
          ("error.rate.limit.exceeded", "超过速率限制"),
          ("error.internal.server", "内部服务器错误"),
          ("error.service.unavailable", "服务暂时不可用")
        ])
      }),
      ("ja-JP", @azimuth.ResourceBundle {
        locale: "ja-JP",
        messages: @azimuth.Map.from_list([
          ("error.connection.timeout", "{0}ミリ秒後に接続タイムアウト"),
          ("error.connection.refused", "接続が拒否されました"),
          ("error.database.unavailable", "データベースが利用できません"),
          ("error.validation.failed", "検証に失敗しました：{0}"),
          ("error.authentication.failed", "認証に失敗しました"),
          ("error.authorization.denied", "アクセスが拒否されました"),
          ("error.resource.not_found", "リソースが見つかりません：{0}"),
          ("error.rate.limit.exceeded", "レート制限を超えました"),
          ("error.internal.server", "内部サーバーエラー"),
          ("error.service.unavailable", "サービスが一時的に利用できません")
        ])
      })
    ])
  }
  
  // 创建错误消息本地化器
  let localizer = @azimuth.ErrorLocalizer {
    i18n_manager: i18n_manager,
    current_locale: "en-US"
  }
  
  // 测试英文错误消息
  let en_timeout_error = localizer.get_message("error.connection.timeout", ["5000"], "en-US")
  let en_refused_error = localizer.get_message("error.connection.refused", [], "en-US")
  let en_validation_error = localizer.get_message("error.validation.failed", ["Invalid input"], "en-US")
  
  assert_eq(en_timeout_error, "Connection timeout after 5000ms")
  assert_eq(en_refused_error, "Connection refused")
  assert_eq(en_validation_error, "Validation failed: Invalid input")
  
  // 测试中文错误消息
  let zh_timeout_error = localizer.get_message("error.connection.timeout", ["5000"], "zh-CN")
  let zh_refused_error = localizer.get_message("error.connection.refused", [], "zh-CN")
  let zh_validation_error = localizer.get_message("error.validation.failed", ["输入无效"], "zh-CN")
  
  assert_eq(zh_timeout_error, "连接超时，已等待5000毫秒")
  assert_eq(zh_refused_error, "连接被拒绝")
  assert_eq(zh_validation_error, "验证失败：输入无效")
  
  // 测试日文错误消息
  let ja_timeout_error = localizer.get_message("error.connection.timeout", ["5000"], "ja-JP")
  let ja_refused_error = localizer.get_message("error.connection.refused", [], "ja-JP")
  let ja_validation_error = localizer.get_message("error.validation.failed", ["無効な入力"], "ja-JP")
  
  assert_eq(ja_timeout_error, "5000ミリ秒後に接続タイムアウト")
  assert_eq(ja_refused_error, "接続が拒否されました")
  assert_eq(ja_validation_error, "検証に失敗しました：無効な入力")
  
  // 测试回退到默认语言（当请求的语言不支持时）
  let unsupported_locale_error = localizer.get_message("error.connection.timeout", ["5000"], "ko-KR")
  assert_eq(unsupported_locale_error, "Connection timeout after 5000ms") // 回退到英文
  
  // 测试消息键不存在时的回退
  let missing_key_error = localizer.get_message("error.unknown.key", [], "zh-CN")
  assert_eq(missing_key_error, "error.unknown.key") // 回退到消息键本身
}

test "日期时间格式本地化" {
  // 创建日期时间本地化器
  let datetime_localizer = @azimuth.DateTimeLocalizer {
    locale_formatters: @azimuth.Map.from_list([
      ("en-US", @azimuth.LocaleFormatter {
        date_format: "MM/dd/yyyy",
        time_format: "HH:mm:ss a",
        datetime_format: "MM/dd/yyyy HH:mm:ss a",
        timezone: "UTC"
      }),
      ("zh-CN", @azimuth.LocaleFormatter {
        date_format: "yyyy年MM月dd日",
        time_format: "HH:mm:ss",
        datetime_format: "yyyy年MM月dd日 HH:mm:ss",
        timezone: "Asia/Shanghai"
      }),
      ("ja-JP", @azimuth.LocaleFormatter {
        date_format: "yyyy/MM/dd",
        time_format: "HH:mm:ss",
        datetime_format: "yyyy/MM/dd HH:mm:ss",
        timezone: "Asia/Tokyo"
      }),
      ("de-DE", @azimuth.LocaleFormatter {
        date_format: "dd.MM.yyyy",
        time_format: "HH:mm:ss",
        datetime_format: "dd.MM.yyyy HH:mm:ss",
        timezone: "Europe/Berlin"
      })
    ])
  }
  
  // 创建测试时间戳
  let test_timestamp = @azimuth.Timestamp(1640995200) // 2022-01-01 00:00:00 UTC
  
  // 测试英文格式
  let en_date = datetime_localizer.format_date(test_timestamp, "en-US")
  let en_time = datetime_localizer.format_time(test_timestamp, "en-US")
  let en_datetime = datetime_localizer.format_datetime(test_timestamp, "en-US")
  
  assert_eq(en_date, "01/01/2022")
  assert_eq(en_time, "00:00:00 AM")
  assert_eq(en_datetime, "01/01/2022 00:00:00 AM")
  
  // 测试中文格式
  let zh_date = datetime_localizer.format_date(test_timestamp, "zh-CN")
  let zh_time = datetime_localizer.format_time(test_timestamp, "zh-CN")
  let zh_datetime = datetime_localizer.format_datetime(test_timestamp, "zh-CN")
  
  assert_eq(zh_date, "2022年01月01日")
  assert_eq(zh_time, "08:00:00") // UTC+8 时区
  assert_eq(zh_datetime, "2022年01月01日 08:00:00")
  
  // 测试日文格式
  let ja_date = datetime_localizer.format_date(test_timestamp, "ja-JP")
  let ja_time = datetime_localizer.format_time(test_timestamp, "ja-JP")
  let ja_datetime = datetime_localizer.format_datetime(test_timestamp, "ja-JP")
  
  assert_eq(ja_date, "2022/01/01")
  assert_eq(ja_time, "09:00:00") // UTC+9 时区
  assert_eq(ja_datetime, "2022/01/01 09:00:00")
  
  // 测试德文格式
  let de_date = datetime_localizer.format_date(test_timestamp, "de-DE")
  let de_time = datetime_localizer.format_time(test_timestamp, "de-DE")
  let de_datetime = datetime_localizer.format_datetime(test_timestamp, "de-DE")
  
  assert_eq(de_date, "01.01.2022")
  assert_eq(de_time, "01:00:00") // UTC+1 时区
  assert_eq(de_datetime, "01.01.2022 01:00:00")
  
  // 测试相对时间格式化
  let now = @azimuth.Timestamp(1640995800) // 10分钟后
  let relative_time = datetime_localizer.format_relative_time(test_timestamp, now, "en-US")
  assert_eq(relative_time, "10 minutes ago")
  
  let relative_time_zh = datetime_localizer.format_relative_time(test_timestamp, now, "zh-CN")
  assert_eq(relative_time_zh, "10分钟前")
  
  let relative_time_ja = datetime_localizer.format_relative_time(test_timestamp, now, "ja-JP")
  assert_eq(relative_time_ja, "10分前")
}

test "数字和货币格式本地化" {
  // 创建数字格式化器
  let number_localizer = @azimuth.NumberLocalizer {
    locale_formatters: @azimuth.Map.from_list([
      ("en-US", @azimuth.NumberFormatter {
        decimal_separator: ".",
        thousands_separator: ",",
        currency_symbol: "$",
        currency_position: "before",
        decimal_places: 2
      }),
      ("zh-CN", @azimuth.NumberFormatter {
        decimal_separator: ".",
        thousands_separator: ",",
        currency_symbol: "¥",
        currency_position: "before",
        decimal_places: 2
      }),
      ("ja-JP", @azimuth.NumberFormatter {
        decimal_separator: ".",
        thousands_separator: ",",
        currency_symbol: "¥",
        currency_position: "before",
        decimal_places: 0
      }),
      ("de-DE", @azimuth.NumberFormatter {
        decimal_separator: ",",
        thousands_separator: ".",
        currency_symbol: "€",
        currency_position: "after",
        decimal_places: 2
      }),
      ("fr-FR", @azimuth.NumberFormatter {
        decimal_separator: ",",
        thousands_separator: " ",
        currency_symbol: "€",
        currency_position: "after",
        decimal_places: 2
      })
    ])
  }
  
  // 测试数字格式化
  let test_number = 1234567.89
  
  // 英文格式
  let en_number = number_localizer.format_number(test_number, "en-US")
  assert_eq(en_number, "1,234,567.89")
  
  // 中文格式
  let zh_number = number_localizer.format_number(test_number, "zh-CN")
  assert_eq(zh_number, "1,234,567.89")
  
  // 德文格式
  let de_number = number_localizer.format_number(test_number, "de-DE")
  assert_eq(de_number, "1.234.567,89")
  
  // 法文格式
  let fr_number = number_localizer.format_number(test_number, "fr-FR")
  assert_eq(fr_number, "1 234 567,89")
  
  // 测试货币格式化
  let test_currency = 1234.56
  
  // 英文货币
  let en_currency = number_localizer.format_currency(test_currency, "en-US")
  assert_eq(en_currency, "$1,234.56")
  
  // 中文货币
  let zh_currency = number_localizer.format_currency(test_currency, "zh-CN")
  assert_eq(zh_currency, "¥1,234.56")
  
  // 日文货币
  let ja_currency = number_localizer.format_currency(test_currency, "ja-JP")
  assert_eq(ja_currency, "¥1,235") // 无小数位，四舍五入
  
  // 德文货币
  let de_currency = number_localizer.format_currency(test_currency, "de-DE")
  assert_eq(de_currency, "1.234,56€")
  
  // 法文货币
  let fr_currency = number_localizer.format_currency(test_currency, "fr-FR")
  assert_eq(fr_currency, "1 234,56€")
  
  // 测试百分比格式化
  let test_percentage = 0.7543
  
  let en_percentage = number_localizer.format_percentage(test_percentage, "en-US")
  assert_eq(en_percentage, "75.43%")
  
  let zh_percentage = number_localizer.format_percentage(test_percentage, "zh-CN")
  assert_eq(zh_percentage, "75.43%")
  
  let de_percentage = number_localizer.format_percentage(test_percentage, "de-DE")
  assert_eq(de_percentage, "75,43%")
}

test "遥测指标名称本地化" {
  // 创建指标名称本地化器
  let metric_localizer = @azimuth.MetricLocalizer {
    locale_translations: @azimuth.Map.from_list([
      ("en-US", @azimuth.Map.from_list([
        ("cpu.usage", "CPU Usage"),
        ("memory.usage", "Memory Usage"),
        ("disk.usage", "Disk Usage"),
        ("network.incoming", "Network Incoming"),
        ("network.outgoing", "Network Outgoing"),
        ("request.count", "Request Count"),
        ("response.time", "Response Time"),
        ("error.rate", "Error Rate"),
        ("throughput", "Throughput"),
        ("availability", "Availability")
      ])),
      ("zh-CN", @azimuth.Map.from_list([
        ("cpu.usage", "CPU使用率"),
        ("memory.usage", "内存使用率"),
        ("disk.usage", "磁盘使用率"),
        ("network.incoming", "网络入流量"),
        ("network.outgoing", "网络出流量"),
        ("request.count", "请求数量"),
        ("response.time", "响应时间"),
        ("error.rate", "错误率"),
        ("throughput", "吞吐量"),
        ("availability", "可用性")
      ])),
      ("ja-JP", @azimuth.Map.from_list([
        ("cpu.usage", "CPU使用率"),
        ("memory.usage", "メモリ使用率"),
        ("disk.usage", "ディスク使用率"),
        ("network.incoming", "ネットワーク受信"),
        ("network.outgoing", "ネットワーク送信"),
        ("request.count", "リクエスト数"),
        ("response.time", "応答時間"),
        ("error.rate", "エラー率"),
        ("throughput", "スループット"),
        ("availability", "可用性")
      ]))
    ])
  }
  
  // 测试指标名称本地化
  let metric_names = [
    "cpu.usage",
    "memory.usage",
    "disk.usage",
    "network.incoming",
    "network.outgoing",
    "request.count",
    "response.time",
    "error.rate",
    "throughput",
    "availability"
  ]
  
  // 测试英文
  let en_metric_names = metric_names.map(fn(name) {
    metric_localizer.get_metric_name(name, "en-US")
  })
  
  assert_eq(en_metric_names[0], "CPU Usage")
  assert_eq(en_metric_names[1], "Memory Usage")
  assert_eq(en_metric_names[2], "Disk Usage")
  assert_eq(en_metric_names[3], "Network Incoming")
  assert_eq(en_metric_names[4], "Network Outgoing")
  assert_eq(en_metric_names[5], "Request Count")
  assert_eq(en_metric_names[6], "Response Time")
  assert_eq(en_metric_names[7], "Error Rate")
  assert_eq(en_metric_names[8], "Throughput")
  assert_eq(en_metric_names[9], "Availability")
  
  // 测试中文
  let zh_metric_names = metric_names.map(fn(name) {
    metric_localizer.get_metric_name(name, "zh-CN")
  })
  
  assert_eq(zh_metric_names[0], "CPU使用率")
  assert_eq(zh_metric_names[1], "内存使用率")
  assert_eq(zh_metric_names[2], "磁盘使用率")
  assert_eq(zh_metric_names[3], "网络入流量")
  assert_eq(zh_metric_names[4], "网络出流量")
  assert_eq(zh_metric_names[5], "请求数量")
  assert_eq(zh_metric_names[6], "响应时间")
  assert_eq(zh_metric_names[7], "错误率")
  assert_eq(zh_metric_names[8], "吞吐量")
  assert_eq(zh_metric_names[9], "可用性")
  
  // 测试日文
  let ja_metric_names = metric_names.map(fn(name) {
    metric_localizer.get_metric_name(name, "ja-JP")
  })
  
  assert_eq(ja_metric_names[0], "CPU使用率")
  assert_eq(ja_metric_names[1], "メモリ使用率")
  assert_eq(ja_metric_names[2], "ディスク使用率")
  assert_eq(ja_metric_names[3], "ネットワーク受信")
  assert_eq(ja_metric_names[4], "ネットワーク送信")
  assert_eq(ja_metric_names[5], "リクエスト数")
  assert_eq(ja_metric_names[6], "応答時間")
  assert_eq(ja_metric_names[7], "エラー率")
  assert_eq(ja_metric_names[8], "スループット")
  assert_eq(ja_metric_names[9], "可用性")
  
  // 测试未知指标名称的回退
  let unknown_metric = metric_localizer.get_metric_name("unknown.metric", "zh-CN")
  assert_eq(unknown_metric, "unknown.metric") // 回退到原始名称
  
  // 测试未知语言的回退
  let unknown_locale = metric_localizer.get_metric_name("cpu.usage", "ko-KR")
  assert_eq(unknown_locale, "cpu.usage") // 回退到原始名称
}

test "仪表盘界面本地化" {
  // 创建仪表盘本地化器
  let dashboard_localizer = @azimuth.DashboardLocalizer {
    locale_resources: @azimuth.Map.from_list([
      ("en-US", @azimuth.DashboardResources {
        title: "Azimuth Telemetry Dashboard",
        navigation: @azimuth.Map.from_list([
          ("overview", "Overview"),
          ("metrics", "Metrics"),
          ("traces", "Traces"),
          ("logs", "Logs"),
          ("alerts", "Alerts"),
          ("settings", "Settings")
        ]),
        common_labels: @azimuth.Map.from_list([
          ("loading", "Loading..."),
          ("error", "Error"),
          ("success", "Success"),
          ("warning", "Warning"),
          ("info", "Information"),
          ("refresh", "Refresh"),
          ("export", "Export"),
          ("filter", "Filter"),
          ("search", "Search"),
          ("close", "Close")
        ]),
        time_ranges: @azimuth.Map.from_list([
          ("1h", "Last 1 Hour"),
          ("6h", "Last 6 Hours"),
          ("24h", "Last 24 Hours"),
          ("7d", "Last 7 Days"),
          ("30d", "Last 30 Days")
        ])
      }),
      ("zh-CN", @azimuth.DashboardResources {
        title: "Azimuth 遥测仪表盘",
        navigation: @azimuth.Map.from_list([
          ("overview", "概览"),
          ("metrics", "指标"),
          ("traces", "追踪"),
          ("logs", "日志"),
          ("alerts", "告警"),
          ("settings", "设置")
        ]),
        common_labels: @azimuth.Map.from_list([
          ("loading", "加载中..."),
          ("error", "错误"),
          ("success", "成功"),
          ("warning", "警告"),
          ("info", "信息"),
          ("refresh", "刷新"),
          ("export", "导出"),
          ("filter", "筛选"),
          ("search", "搜索"),
          ("close", "关闭")
        ]),
        time_ranges: @azimuth.Map.from_list([
          ("1h", "最近1小时"),
          ("6h", "最近6小时"),
          ("24h", "最近24小时"),
          ("7d", "最近7天"),
          ("30d", "最近30天")
        ])
      }),
      ("ja-JP", @azimuth.DashboardResources {
        title: "Azimuth テレメトリーダッシュボード",
        navigation: @azimuth.Map.from_list([
          ("overview", "概要"),
          ("metrics", "メトリック"),
          ("traces", "トレース"),
          ("logs", "ログ"),
          ("alerts", "アラート"),
          ("settings", "設定")
        ]),
        common_labels: @azimuth.Map.from_list([
          ("loading", "読み込み中..."),
          ("error", "エラー"),
          ("success", "成功"),
          ("warning", "警告"),
          ("info", "情報"),
          ("refresh", "更新"),
          ("export", "エクスポート"),
          ("filter", "フィルター"),
          ("search", "検索"),
          ("close", "閉じる")
        ]),
        time_ranges: @azimuth.Map.from_list([
          ("1h", "過去1時間"),
          ("6h", "過去6時間"),
          ("24h", "過去24時間"),
          ("7d", "過去7日間"),
          ("30d", "過去30日間")
        ])
      })
    ])
  }
  
  // 测试仪表盘标题本地化
  let en_title = dashboard_localizer.get_dashboard_title("en-US")
  let zh_title = dashboard_localizer.get_dashboard_title("zh-CN")
  let ja_title = dashboard_localizer.get_dashboard_title("ja-JP")
  
  assert_eq(en_title, "Azimuth Telemetry Dashboard")
  assert_eq(zh_title, "Azimuth 遥测仪表盘")
  assert_eq(ja_title, "Azimuth テレメトリーダッシュボード")
  
  // 测试导航菜单本地化
  let navigation_items = ["overview", "metrics", "traces", "logs", "alerts", "settings"]
  
  let en_navigation = navigation_items.map(fn(item) {
    dashboard_localizer.get_navigation_label(item, "en-US")
  })
  
  let zh_navigation = navigation_items.map(fn(item) {
    dashboard_localizer.get_navigation_label(item, "zh-CN")
  })
  
  let ja_navigation = navigation_items.map(fn(item) {
    dashboard_localizer.get_navigation_label(item, "ja-JP")
  })
  
  // 验证英文导航
  assert_eq(en_navigation[0], "Overview")
  assert_eq(en_navigation[1], "Metrics")
  assert_eq(en_navigation[2], "Traces")
  assert_eq(en_navigation[3], "Logs")
  assert_eq(en_navigation[4], "Alerts")
  assert_eq(en_navigation[5], "Settings")
  
  // 验证中文导航
  assert_eq(zh_navigation[0], "概览")
  assert_eq(zh_navigation[1], "指标")
  assert_eq(zh_navigation[2], "追踪")
  assert_eq(zh_navigation[3], "日志")
  assert_eq(zh_navigation[4], "告警")
  assert_eq(zh_navigation[5], "设置")
  
  // 验证日文导航
  assert_eq(ja_navigation[0], "概要")
  assert_eq(ja_navigation[1], "メトリック")
  assert_eq(ja_navigation[2], "トレース")
  assert_eq(ja_navigation[3], "ログ")
  assert_eq(ja_navigation[4], "アラート")
  assert_eq(ja_navigation[5], "設定")
  
  // 测试通用标签本地化
  let common_labels = ["loading", "error", "success", "warning", "info", "refresh", "export", "filter", "search", "close"]
  
  let en_labels = common_labels.map(fn(label) {
    dashboard_localizer.get_common_label(label, "en-US")
  })
  
  let zh_labels = common_labels.map(fn(label) {
    dashboard_localizer.get_common_label(label, "zh-CN")
  })
  
  let ja_labels = common_labels.map(fn(label) {
    dashboard_localizer.get_common_label(label, "ja-JP")
  })
  
  // 验证部分英文标签
  assert_eq(en_labels[0], "Loading...")
  assert_eq(en_labels[1], "Error")
  assert_eq(en_labels[2], "Success")
  assert_eq(en_labels[3], "Warning")
  assert_eq(en_labels[4], "Information")
  
  // 验证部分中文标签
  assert_eq(zh_labels[0], "加载中...")
  assert_eq(zh_labels[1], "错误")
  assert_eq(zh_labels[2], "成功")
  assert_eq(zh_labels[3], "警告")
  assert_eq(zh_labels[4], "信息")
  
  // 验证部分日文标签
  assert_eq(ja_labels[0], "読み込み中...")
  assert_eq(ja_labels[1], "エラー")
  assert_eq(ja_labels[2], "成功")
  assert_eq(ja_labels[3], "警告")
  assert_eq(ja_labels[4], "情報")
  
  // 测试时间范围本地化
  let time_ranges = ["1h", "6h", "24h", "7d", "30d"]
  
  let en_time_ranges = time_ranges.map(fn(range) {
    dashboard_localizer.get_time_range_label(range, "en-US")
  })
  
  let zh_time_ranges = time_ranges.map(fn(range) {
    dashboard_localizer.get_time_range_label(range, "zh-CN")
  })
  
  let ja_time_ranges = time_ranges.map(fn(range) {
    dashboard_localizer.get_time_range_label(range, "ja-JP")
  })
  
  // 验证英文时间范围
  assert_eq(en_time_ranges[0], "Last 1 Hour")
  assert_eq(en_time_ranges[1], "Last 6 Hours")
  assert_eq(en_time_ranges[2], "Last 24 Hours")
  assert_eq(en_time_ranges[3], "Last 7 Days")
  assert_eq(en_time_ranges[4], "Last 30 Days")
  
  // 验证中文时间范围
  assert_eq(zh_time_ranges[0], "最近1小时")
  assert_eq(zh_time_ranges[1], "最近6小时")
  assert_eq(zh_time_ranges[2], "最近24小时")
  assert_eq(zh_time_ranges[3], "最近7天")
  assert_eq(zh_time_ranges[4], "最近30天")
  
  // 验证日文时间范围
  assert_eq(ja_time_ranges[0], "過去1時間")
  assert_eq(ja_time_ranges[1], "過去6時間")
  assert_eq(ja_time_ranges[2], "過去24時間")
  assert_eq(ja_time_ranges[3], "過去7日間")
  assert_eq(ja_time_ranges[4], "過去30日間")
}