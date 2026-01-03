// Azimuth 额外测试用例
// 涵盖更多MoonBit语言特性和Azimuth功能的测试用例

// 测试1: 异步操作和并发处理
test "异步操作和并发处理测试" {
  // 创建异步任务
  let task1 = async {
    let mut result = 0
    for i in 1..=100 {
      result = result + i
    }
    result
  }
  
  let task2 = async {
    let mut result = 1
    for i in 1..=10 {
      result = result * i
    }
    result
  }
  
  // 等待任务完成
  let sum_result = await task1
  let factorial_result = await task2
  
  // 验证结果
  assert_eq(sum_result, 5050)  // 1到100的和
  assert_eq(factorial_result, 3628800)  // 10的阶乘
}

// 测试2: 错误处理和异常恢复
test "错误处理和异常恢复测试" {
  // 定义可能失败的函数
  let divide = fn(a, b) {
    if b == 0 {
      Err("Division by zero")
    } else {
      Ok(a / b)
    }
  }
  
  // 测试成功情况
  let success_result = divide(10, 2)
  match success_result {
    Ok(value) => assert_eq(value, 5)
    Err(_) => assert_true(false)
  }
  
  // 测试错误情况
  let error_result = divide(10, 0)
  match error_result {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Division by zero")
  }
  
  // 测试错误恢复
  let recovered_result = match error_result {
    Ok(value) => value
    Err(_) => 0  // 默认值
  }
  assert_eq(recovered_result, 0)
}

// 测试3: 高阶函数和函数组合
test "高阶函数和函数组合测试" {
  // 定义基础函数
  let add = fn(x, y) { x + y }
  let multiply = fn(x, y) { x * y }
  let square = fn(x) { x * x }
  
  // 定义高阶函数
  let apply_twice = fn(f, x) { f(f(x)) }
  let compose = fn(f, g) { fn(x) { f(g(x)) } }
  
  // 测试apply_twice
  assert_eq(apply_twice(fn(x) { x + 1 }, 5), 7)
  assert_eq(apply_twice(square, 3), 81)  // (3^2)^2 = 81
  
  // 测试函数组合
  let add_then_square = compose(square, fn(x) { x + 1 })
  assert_eq(add_then_square(4), 25)  // (4+1)^2 = 25
  
  let square_then_add = compose(fn(x) { x + 1 }, square)
  assert_eq(square_then_add(4), 17)  // (4^2)+1 = 17
}

// 测试4: 数据结构操作
test "数据结构操作测试" {
  // 创建链表
  let list = List::empty()
  let list1 = List::push(list, 1)
  let list2 = List::push(list1, 2)
  let list3 = List::push(list2, 3)
  
  // 测试链表操作
  assert_eq(List::length(list3), 3)
  assert_eq(List::head(list3), Some(3))
  assert_eq(List::tail(list3), Some(list2))
  
  // 测试链表转换
  let doubled = List::map(list3, fn(x) { x * 2 })
  assert_eq(List::to_array(doubled), [6, 4, 2])
  
  // 测试链表过滤
  let evens = List::filter(list3, fn(x) { x % 2 == 0 })
  assert_eq(List::to_array(evens), [2])
  
  // 创建字典/映射
  let map = Map::empty()
  let map1 = Map::insert(map, "key1", "value1")
  let map2 = Map::insert(map1, "key2", "value2")
  let map3 = Map::insert(map2, "key3", "value3")
  
  // 测试映射操作
  assert_eq(Map::get(map3, "key1"), Some("value1"))
  assert_eq(Map::get(map3, "key2"), Some("value2"))
  assert_eq(Map::get(map3, "key4"), None)
  assert_eq(Map::size(map3), 3)
}

// 测试5: 时间和日期操作
test "时间和日期操作测试" {
  // 创建时间点
  let now = Time::now()
  let later = Time::add_seconds(now, 3600)  // 1小时后
  
  // 测试时间比较
  assert_true(Time::after(later, now))
  assert_false(Time::after(now, later))
  assert_true(Time::before(now, later))
  
  // 测试时间差
  let duration = Time::diff(later, now)
  assert_eq(duration, 3600)
  
  // 创建日期
  let date = Date::new(2026, 1, 3)
  let tomorrow = Date::add_days(date, 1)
  let next_week = Date::add_days(date, 7)
  
  // 测试日期操作
  assert_eq(Date::year(tomorrow), 2026)
  assert_eq(Date::month(tomorrow), 1)
  assert_eq(Date::day(tomorrow), 4)
  
  assert_eq(Date::year(next_week), 2026)
  assert_eq(Date::month(next_week), 1)
  assert_eq(Date::day(next_week), 10)
  
  // 测试日期格式化
  let formatted = Date::format(date, "%Y-%m-%d")
  assert_eq(formatted, "2026-01-03")
}

// 测试6: 字符串处理和正则表达式
test "字符串处理和正则表达式测试" {
  let text = "Azimuth telemetry system v1.2.3"
  
  // 测试基本字符串操作
  assert_eq(text.length(), 28)
  assert_true(text.starts_with("Azimuth"))
  assert_true(text.ends_with("v1.2.3"))
  assert_true(text.contains("telemetry"))
  
  // 测试字符串分割
  let words = text.split(" ")
  assert_eq(words, ["Azimuth", "telemetry", "system", "v1.2.3"])
  
  // 测试字符串替换
  let replaced = text.replace("v1.2.3", "v2.0.0")
  assert_eq(replaced, "Azimuth telemetry system v2.0.0")
  
  // 测试正则表达式
  let pattern = Regex::new(r"\d+\.\d+\.\d+")
  assert_true(Regex::matches(pattern, text))
  
  let version = Regex::find_first(pattern, text)
  assert_eq(version, Some("1.2.3"))
  
  // 测试正则替换
  let masked = Regex::replace(pattern, text, "X.X.X")
  assert_eq(masked, "Azimuth telemetry system X.X.X")
}

// 测试7: 文件系统操作
test "文件系统操作测试" {
  // 创建临时文件路径
  let temp_path = "/tmp/azimuth_test.txt"
  
  // 写入文件
  let content = "Azimuth test content\nLine 2\nLine 3"
  let write_result = File::write_string(temp_path, content)
  assert_true(write_result)
  
  // 读取文件
  let read_result = File::read_string(temp_path)
  match read_result {
    Ok(read_content) => assert_eq(read_content, content)
    Err(_) => assert_true(false)
  }
  
  // 检查文件存在
  assert_true(File::exists(temp_path))
  
  // 获取文件大小
  let size = File::size(temp_path)
  assert_eq(size, content.length())
  
  // 清理临时文件
  let delete_result = File::delete(temp_path)
  assert_true(delete_result)
  assert_false(File::exists(temp_path))
}

// 测试8: 网络通信和HTTP请求
test "网络通信和HTTP请求测试" {
  // 创建HTTP客户端
  let client = HttpClient::new()
  
  // 设置请求头
  let headers = [
    ("Content-Type", "application/json"),
    ("User-Agent", "Azimuth-Telemetry/1.0")
  ]
  
  // 创建请求体
  let body = "{\"message\": \"test\", \"timestamp\": 1672531200}"
  
  // 发送POST请求（模拟）
  let request = HttpRequest::new("https://api.example.com/telemetry")
    .method("POST")
    .headers(headers)
    .body(body)
  
  // 模拟响应
  let response = HttpResponse::new()
    .status(200)
    .header("Content-Type", "application/json")
    .body("{\"status\": \"success\", \"id\": \"12345\"}")
  
  // 验证响应
  assert_eq(response.status(), 200)
  assert_eq(response.header("Content-Type"), Some("application/json"))
  
  // 解析JSON响应
  let json = Json::parse(response.body())
  match json {
    Ok(parsed) => {
      assert_eq(Json::get_string(parsed, "status"), Some("success"))
      assert_eq(Json::get_string(parsed, "id"), Some("12345"))
    }
    Err(_) => assert_true(false)
  }
}

// 测试9: 加密和哈希操作
test "加密和哈希操作测试" {
  let message = "Azimuth telemetry data"
  
  // 测试MD5哈希
  let md5_hash = Hash::md5(message)
  assert_eq(md5_hash.length(), 32)  // MD5哈希长度为32个十六进制字符
  
  // 测试SHA256哈希
  let sha256_hash = Hash::sha256(message)
  assert_eq(sha256_hash.length(), 64)  // SHA256哈希长度为64个十六进制字符
  
  // 测试HMAC
  let secret = "azimuth-secret-key"
  let hmac = Hash::hmac_sha256(message, secret)
  assert_eq(hmac.length(), 64)
  
  // 测试Base64编码/解码
  let encoded = Base64::encode(message)
  let decoded = Base64::decode(encoded)
  assert_eq(decoded, Some(message))
  
  // 测试简单加密（模拟）
  let key = "encryption-key"
  let encrypted = Crypto::simple_encrypt(message, key)
  let decrypted = Crypto::simple_decrypt(encrypted, key)
  assert_eq(decrypted, Some(message))
  
  // 测试加密失败情况
  let wrong_key = "wrong-key"
  let failed_decrypt = Crypto::simple_decrypt(encrypted, wrong_key)
  assert_eq(failed_decrypt, None)
}

// 测试10: 性能基准测试
test "性能基准测试测试" {
  // 测试数组操作性能
  let large_array = Array::range(1, 10000)  // 创建1到10000的数组
  
  // 测试map操作
  let start_time = Time::now()
  let mapped = large_array.map(fn(x) { x * 2 })
  let map_duration = Time::diff(Time::now(), start_time)
  
  // 验证结果正确性
  assert_eq(mapped.length(), 10000)
  assert_eq(mapped[0], 2)
  assert_eq(mapped[9999], 20000)
  
  // 测试filter操作
  start_time = Time::now()
  let filtered = large_array.filter(fn(x) { x % 2 == 0 })
  let filter_duration = Time::diff(Time::now(), start_time)
  
  // 验证结果正确性
  assert_eq(filtered.length(), 5000)
  assert_eq(filtered[0], 2)
  assert_eq(filtered[4999], 10000)
  
  // 测试reduce操作
  start_time = Time::now()
  let sum = large_array.reduce(fn(acc, x) { acc + x }, 0)
  let reduce_duration = Time::diff(Time::now(), start_time)
  
  // 验证结果正确性
  assert_eq(sum, 50005000)  // 1到10000的和
  
  // 验证性能在合理范围内（这里只是示例，实际阈值需要根据环境调整）
  assert_true(map_duration < 1000)  // map操作应在1秒内完成
  assert_true(filter_duration < 1000)  // filter操作应在1秒内完成
  assert_true(reduce_duration < 1000)  // reduce操作应在1秒内完成
  
  // 记录性能指标
  let perf_metrics = [
    ("array.map.duration_ms", FloatValue(map_duration)),
    ("array.filter.duration_ms", FloatValue(filter_duration)),
    ("array.reduce.duration_ms", FloatValue(reduce_duration)),
    ("array.size", IntValue(10000))
  ]
  
  // 验证性能指标
  assert_eq(perf_metrics.length(), 4)
}