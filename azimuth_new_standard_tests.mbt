// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含全面的测试覆盖

// 导入必要的模块
use azimuth

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @assertion.assert_eq(5, azimuth.add(2, 3))?
  @assertion.assert_eq(6, azimuth.multiply(2, 3))?
  @assertion.assert_eq(8, azimuth.add(10, -2))?
  @assertion.assert_eq(-6, azimuth.multiply(2, -3))?
  @assertion.assert_eq(0, azimuth.multiply(5, 0))?
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  @assertion.assert_eq_string("Hello, World!", azimuth.greet("World"))?
  @assertion.assert_eq_string("Hello, Alice!", azimuth.greet("Alice"))?
  @assertion.assert_eq_string("Hello, Bob!", azimuth.greet("Bob"))?
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))?
}

test "mathematical_formulas" {
  // 数学公式测试
  // 计算矩形面积：长 × 宽
  let length = 8
  let width = 5
  let area = azimuth.multiply(length, width)
  @assertion.assert_eq(40, area)?
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = azimuth.multiply(base, height) / 2
  @assertion.assert_eq(30, triangle_area)?
  
  // 计算平均数：(a + b + c) ÷ 3
  let a = 12
  let b = 18
  let c = 24
  let average = azimuth.add(azimuth.add(a, b), c) / 3
  @assertion.assert_eq(18, average)?
}

test "business_calculations" {
  // 商业计算测试
  // 计算总价：单价 × 数量
  let unit_price = 25
  let quantity = 4
  let total_price = azimuth.multiply(unit_price, quantity)
  @assertion.assert_eq(100, total_price)?
  
  // 计算折扣后价格：原价 - 折扣金额
  let original_price = 200
  let discount_amount = 30
  let discounted_price = azimuth.add(original_price, -discount_amount)
  @assertion.assert_eq(170, discounted_price)?
  
  // 计算税费：价格 × 税率 ÷ 100
  let price = 150
  let tax_rate = 10
  let tax_amount = azimuth.multiply(price, tax_rate) / 100
  @assertion.assert_eq(15, tax_amount)?
}

test "temperature_conversion" {
  // 温度转换测试
  // 摄氏度转华氏度：F = C × 9/5 + 32
  let celsius = 20
  let fahrenheit = azimuth.multiply(azimuth.multiply(celsius, 9), 1) / 5 + 32
  @assertion.assert_eq(68, fahrenheit)?
  
  // 华氏度转摄氏度：C = (F - 32) × 5/9
  let fahrenheit2 = 100
  let celsius2 = azimuth.multiply(azimuth.add(fahrenheit2, -32), 5) / 9
  @assertion.assert_eq(37, celsius2)?  // 整数除法结果
}

test "array_and_sequence_operations" {
  // 数组和序列操作测试
  // 计算等差数列前n项和：n/2 × (首项 + 末项)
  let n = 6
  let first_term = 3
  let last_term = 13
  let sum = azimuth.multiply(n, azimuth.add(first_term, last_term)) / 2
  @assertion.assert_eq(48, sum)?  // 6/2 × (3 + 13) = 3 × 16 = 48
  
  // 计算阶乘：n! = n × (n-1) × ... × 1
  let num = 5
  let factorial = azimuth.multiply(num, azimuth.multiply(num - 1, azimuth.multiply(num - 2, azimuth.multiply(num - 3, num - 4))))
  @assertion.assert_eq(120, factorial)?
}

test "financial_calculations" {
  // 金融计算测试
  // 计算简单利息：本金 × 利率 × 时间
  let principal = 1000
  let rate = 5
  let time = 3
  let simple_interest = azimuth.multiply(azimuth.multiply(principal, rate), time) / 100
  @assertion.assert_eq(150, simple_interest)?
  
  // 计算复利：本金 × (1 + 利率)^时间
  let compound_interest = azimuth.multiply(principal, azimuth.add(100, rate)) / 100
  @assertion.assert_eq(1050, compound_interest)?  // 一年后的金额
  
  // 计算贷款月供：简化计算
  let loan_amount = 12000
  let months = 12
  let monthly_payment = loan_amount / months
  @assertion.assert_eq(1000, monthly_payment)?
}

test "physics_calculations" {
  // 物理计算测试
  // 计算速度：距离 ÷ 时间
  let distance = 300
  let time = 5
  let speed = distance / time
  @assertion.assert_eq(60, speed)?
  
  // 计算加速度：(末速度 - 初速度) ÷ 时间
  let final_velocity = 30
  let initial_velocity = 10
  let acceleration = azimuth.add(final_velocity, -initial_velocity) / time
  @assertion.assert_eq(4, acceleration)?
  
  // 计算力：质量 × 加速度
  let mass = 10
  let force = azimuth.multiply(mass, acceleration)
  @assertion.assert_eq(40, force)?
}

test "boundary_and_edge_cases" {
  // 边界和边缘情况测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试零值运算
  @assertion.assert_eq(0, azimuth.add(0, 0))?
  @assertion.assert_eq(0, azimuth.multiply(0, 0))?
  @assertion.assert_eq(0, azimuth.multiply(0, max_int))?
  @assertion.assert_eq(0, azimuth.multiply(0, min_int))?
  
  // 测试单位值运算
  @assertion.assert_eq(1, azimuth.multiply(1, 1))?
  @assertion.assert_eq(max_int, azimuth.multiply(max_int, 1))?
  @assertion.assert_eq(min_int, azimuth.multiply(min_int, 1))?
  
  // 测试负值运算
  @assertion.assert_eq(-1, azimuth.add(0, -1))?
  @assertion.assert_eq(1, azimuth.add(0, 1))?
  @assertion.assert_eq(-5, azimuth.multiply(5, -1))?
}

test "complex_problem_solving" {
  // 复杂问题解决测试
  // 鸡兔同笼问题：已知总头数和总脚数，求鸡兔数量
  let total_heads = 10
  let total_legs = 32
  
  // 设鸡有x只，兔有y只
  // x + y = 10, 2x + 4y = 32
  // 解得：y = (32 - 2×10) / 2 = 6, x = 10 - 6 = 4
  let rabbits = azimuth.add(total_legs, azimuth.multiply(-2, total_heads)) / 2
  let chickens = azimuth.add(total_heads, -rabbits)
  @assertion.assert_eq(6, rabbits)?
  @assertion.assert_eq(4, chickens)?
  
  // 百钱买百鸡问题：公鸡5元一只，母鸡3元一只，小鸡1元三只
  // 5x + 3y + z/3 = 100, x + y + z = 100
  // 一个简单解：x=4, y=18, z=78
  let roosters = 4
  let hens = 18
  let chicks = 78
  let total_cost = azimuth.add(azimuth.add(azimuth.multiply(5, roosters), azimuth.multiply(3, hens)), chicks / 3)
  let total_count = azimuth.add(azimuth.add(roosters, hens), chicks)
  @assertion.assert_eq(100, total_cost)?
  @assertion.assert_eq(100, total_count)?
}