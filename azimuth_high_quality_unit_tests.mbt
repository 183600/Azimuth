// Azimuth 项目高质量单元测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能和边界情况

test "add_function_comprehensive" {
  // 全面测试加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
}

test "greet_function_various_inputs" {
  // 测试问候功能的各种输入
  inspect(MoonBit!", @azimuth.greet("MoonBit", "Hello)?)
  inspect(World!", @azimuth.greet("World", "Hello)?)
  inspect(!", @azimuth.greet("", "Hello)?)
  inspect(测试!", @azimuth.greet("测试", "Hello)?)
  inspect(123!", @azimuth.greet("123", "Hello)?)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 3
  
  // 交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 结合律：(a + b) + c = a + (b + c)
  let c = 5
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 分配律：a × (b + c) = a × b + a × c
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "complex_business_calculations" {
  // 测试复杂商业计算
  let unit_price = 25
  let quantity = 8
  let discount_rate = 10  // 10% 折扣
  let tax_rate = 5        // 5% 税
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算折扣
  let discount = @azimuth.multiply(subtotal, discount_rate) / 100
  
  // 计算折后价格
  let discounted_price = @azimuth.add(subtotal, -discount)
  
  // 计算税费
  let tax = @azimuth.multiply(discounted_price, tax_rate) / 100
  
  // 计算总价
  let total = @azimuth.add(discounted_price, tax)
  
  @azimuth.assert_eq(200, subtotal)      // 25 × 8 = 200
  @azimuth.assert_eq(20, discount)       // 200 × 10% = 20
  @azimuth.assert_eq(180, discounted_price)  // 200 - 20 = 180
  @azimuth.assert_eq(9, tax)             // 180 × 5% = 9
  @azimuth.assert_eq(189, total)         // 180 + 9 = 189
}

test "edge_cases_and_boundary_values" {
  // 测试边界值和特殊情况
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试大数运算
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 0))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, 0))
  
  // 测试零的性质
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_int))
  @azimuth.assert_eq(0, @azimuth.multiply(min_int, 0))
  
  // 测试1的性质
  @azimuth.assert_eq(max_int, @azimuth.multiply(max_int, 1))
  @azimuth.assert_eq(min_int, @azimuth.multiply(min_int, 1))
}

test "coordinate_geometry_calculations" {
  // 测试坐标几何计算
  let x1 = 1
  let y1 = 2
  let x2 = 5
  let y2 = 6
  
  // 计算x和y的差值
  let dx = @azimuth.add(x2, -x1)
  let dy = @azimuth.add(y2, -y1)
  
  // 计算距离的平方（避免开方运算）
  let distance_squared = @azimuth.add(@azimuth.multiply(dx, dx), @azimuth.multiply(dy, dy))
  
  // 计算中点坐标
  let midpoint_x = @azimuth.add(x1, x2) / 2
  let midpoint_y = @azimuth.add(y1, y2) / 2
  
  @azimuth.assert_eq(4, dx)              // 5 - 1 = 4
  @azimuth.assert_eq(4, dy)              // 6 - 2 = 4
  @azimuth.assert_eq(32, distance_squared)  // 4² + 4² = 16 + 16 = 32
  @azimuth.assert_eq(3, midpoint_x)      // (1 + 5) / 2 = 3
  @azimuth.assert_eq(4, midpoint_y)      // (2 + 6) / 2 = 4
}

test "time_calculations" {
  // 测试时间计算
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 将所有时间转换为秒
  let total_seconds = @azimuth.add(@azimuth.multiply(hours, 3600), @azimuth.add(@azimuth.multiply(minutes, 60), seconds))
  
  // 计算总分钟数（向下取整）
  let total_minutes = total_seconds / 60
  
  @azimuth.assert_eq(9930, total_seconds)  // 2×3600 + 45×60 + 30 = 7200 + 2700 + 30 = 9930
  @azimuth.assert_eq(165, total_minutes)   // 9930 / 60 = 165.5 → 165
}

test "series_and_pattern_calculations" {
  // 测试级数和模式计算
  // 计算前n个自然数的和：n(n+1)/2
  let n = 10
  let sum_n = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  
  // 计算前n个奇数的和：n²
  let sum_odds = @azimuth.multiply(n, n)
  
  // 计算前n个偶数的和：n(n+1)
  let sum_evens = @azimuth.multiply(n, @azimuth.add(n, 1))
  
  @azimuth.assert_eq(55, sum_n)     // 10×11/2 = 55
  @azimuth.assert_eq(100, sum_odds) // 10² = 100
  @azimuth.assert_eq(110, sum_evens) // 10×11 = 110
}

test "error_handling_and_special_cases" {
  // 测试错误处理和特殊情况
  // 测试空字符串
  inspect(!", @azimuth.greet("", "Hello)?)
  
  // 测试单字符
  inspect(A!", @azimuth.greet("A", "Hello)?)
  
  // 测试包含空格的字符串
  inspect(Hello World!", @azimuth.greet("Hello World", "Hello)?)
  
  // 测试数学运算的特殊情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
}