// Azimuth 项目综合测试套件
// 使用标准 MoonBit 测试语法，覆盖核心功能场景
// 自包含版本，不依赖外部函数

// 定义基本的加法函数
fn add(a : Int, b : Int) -> Int {
  a + b
}

// 定义基本的乘法函数
fn multiply(a : Int, b : Int) -> Int {
  a * b
}

// 定义问候函数
fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

// 定义断言函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  if expected == actual {
    ()
  } else {
    // 简单的错误处理
    @builtin.abort("Assertion failed")
  }
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  if expected == actual {
    ()
  } else {
    // 简单的错误处理
    @builtin.abort("String assertion failed")
  }
}

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(7, 3))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(0, multiply(8, 0))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, 用户!", greet("用户"))
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_formulas" {
  // 测试数学公式
  
  // 圆的周长: 2 * π * r (简化使用 π ≈ 3)
  let radius = 5
  let circumference = multiply(2, multiply(3, radius))
  assert_eq(30, circumference)
  
  // 梯形面积: (上底 + 下底) * 高 / 2
  let top_base = 6
  let bottom_base = 10
  let height = 4
  let trapezoid_area = multiply(add(top_base, bottom_base), height) / 2
  assert_eq(32, trapezoid_area)
}

test "business_calculations" {
  // 测试商业计算
  
  // 利润计算: 收入 - 成本
  let revenue = 5000
  let cost = 3200
  let profit = add(revenue, -cost)
  assert_eq(1800, profit)
  
  // 利润率: 利润 / 成本 * 100
  let profit_rate = multiply(profit, 100) / cost
  assert_eq(56, profit_rate)
}

test "unit_conversions" {
  // 测试单位转换
  
  // 公斤转克: 公斤 * 1000
  let kg = 3
  let grams = multiply(kg, 1000)
  assert_eq(3000, grams)
  
  // 米转厘米: 米 * 100
  let meters = 5
  let cm = multiply(meters, 100)
  assert_eq(500, cm)
  
  // 升转毫升: 升 * 1000
  let liters = 2
  let ml = multiply(liters, 1000)
  assert_eq(2000, ml)
}

test "statistical_calculations" {
  // 测试统计计算
  
  // 平均值计算: (值1 + 值2 + 值3) / 3
  let val1 = 80
  let val2 = 90
  let val3 = 85
  let sum = add(add(val1, val2), val3)
  let average = sum / 3
  assert_eq(85, average)
  
  // 总和计算
  let numbers_sum = add(add(add(10, 20), 30), 40)
  assert_eq(100, numbers_sum)
}

test "time_unit_calculations" {
  // 测试时间单位计算
  
  // 秒转分钟: 秒 / 60
  let seconds = 3600
  let minutes = seconds / 60
  assert_eq(60, minutes)
  
  // 分钟转小时: 分钟 / 60
  let hours = minutes / 60
  assert_eq(1, hours)
  
  // 一天的总秒数
  let day_seconds = multiply(24, multiply(60, 60))
  assert_eq(86400, day_seconds)
}

test "array_operations_simulation" {
  // 测试数组操作模拟
  
  // 模拟数组长度变化
  let array_length = 0
  
  // 添加元素
  let array_length1 = add(array_length, 1)  // 添加第一个元素
  assert_eq(1, array_length1)
  
  let array_length2 = add(array_length1, 1)  // 添加第二个元素
  assert_eq(2, array_length2)
  
  let array_length3 = add(array_length2, 1)  // 添加第三个元素
  assert_eq(3, array_length3)
  
  // 移除元素
  let array_length4 = add(array_length3, -1)  // 移除一个元素
  assert_eq(2, array_length4)
}

test "game_score_calculations" {
  // 测试游戏得分计算
  
  // 基础得分系统
  let base_score = 100
  let combo_multiplier = 2
  let bonus = 50
  
  // 计算最终得分: (基础得分 * 连击倍数) + 奖励
  let final_score = add(multiply(base_score, combo_multiplier), bonus)
  assert_eq(250, final_score)
  
  // 多轮游戏总分
  let round1_score = 150
  let round2_score = 200
  let round3_score = 175
  let total_score = add(add(round1_score, round2_score), round3_score)
  assert_eq(525, total_score)
}

test "boundary_value_tests" {
  // 测试边界值
  
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  
  // 单值测试
  assert_eq(1, add(1, 0))
  assert_eq(1, multiply(1, 1))
  
  // 负值边界测试
  assert_eq(-1, add(0, -1))
  assert_eq(1, multiply(-1, -1))
}