// Azimuth 项目高质量单元测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能和边界情况

test "add_function_comprehensive" {
  // 全面测试加法功能
  inspect(@azimuth.add(2, 3), "5")?
  inspect(@azimuth.add(0, 0), "0")?
  inspect(@azimuth.add(-2, -3), "-5")?
  inspect(@azimuth.add(-5, 5), "0")?
  inspect(@azimuth.add(50, 50), "100")?
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  inspect(@azimuth.multiply(2, 3), "6")?
  inspect(@azimuth.multiply(0, 5), "0")?
  inspect(@azimuth.multiply(-2, 3), "-6")?
  inspect(@azimuth.multiply(-2, -3), "6")?
  inspect(@azimuth.multiply(1, 1), "1")?
}

test "greet_function_various_inputs" {
  // 测试问候功能的各种输入
  inspect(@azimuth.greet("MoonBit"), "Hello, MoonBit!")?
  inspect(@azimuth.greet("World"), "Hello, World!")?
  inspect(@azimuth.greet(""), "Hello, !")?
  inspect(@azimuth.greet("测试"), "Hello, 测试!")?
  inspect(@azimuth.greet("123"), "Hello, 123!")?
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 3
  
  // 交换律：a + b = b + a
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(b, a)
  inspect(sum1, sum2.to_string())?
  
  // 结合律：(a + b) + c = a + (b + c)
  let c = 5
  let sum3 = @azimuth.add(@azimuth.add(a, b), c)
  let sum4 = @azimuth.add(a, @azimuth.add(b, c))
  inspect(sum3, sum4.to_string())?
  
  // 分配律：a × (b + c) = a × b + a × c
  let product1 = @azimuth.multiply(a, @azimuth.add(b, c))
  let product2 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  inspect(product1, product2.to_string())?
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
  
  inspect(subtotal, "200")?      // 25 × 8 = 200
  inspect(discount, "20")?       // 200 × 10% = 20
  inspect(discounted_price, "180")?  // 200 - 20 = 180
  inspect(tax, "9")?             // 180 × 5% = 9
  inspect(total, "189")?         // 180 + 9 = 189
}

test "edge_cases_and_boundary_values" {
  // 测试边界值和特殊情况
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试大数运算
  inspect(@azimuth.add(max_int, 0), "2147483647")?
  inspect(@azimuth.add(min_int, 0), "-2147483648")?
  
  // 测试零的性质
  inspect(@azimuth.multiply(0, max_int), "0")?
  inspect(@azimuth.multiply(min_int, 0), "0")?
  
  // 测试1的性质
  inspect(@azimuth.multiply(max_int, 1), "2147483647")?
  inspect(@azimuth.multiply(min_int, 1), "-2147483648")?
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
  
  inspect(dx, "4")?              // 5 - 1 = 4
  inspect(dy, "4")?              // 6 - 2 = 4
  inspect(distance_squared, "32")?  // 4² + 4² = 16 + 16 = 32
  inspect(midpoint_x, "3")?      // (1 + 5) / 2 = 3
  inspect(midpoint_y, "4")?      // (2 + 6) / 2 = 4
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
  
  inspect(total_seconds, "9930")?  // 2×3600 + 45×60 + 30 = 7200 + 2700 + 30 = 9930
  inspect(total_minutes, "165")?   // 9930 / 60 = 165.5 → 165
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
  
  inspect(sum_n, "55")?     // 10×11/2 = 55
  inspect(sum_odds, "100")? // 10² = 100
  inspect(sum_evens, "110")? // 10×11 = 110
}

test "error_handling_and_special_cases" {
  // 测试错误处理和特殊情况
  // 测试空字符串
  inspect(@azimuth.greet(""), "Hello, !")?
  
  // 测试单字符
  inspect(@azimuth.greet("A"), "Hello, A!")?
  
  // 测试包含空格的字符串
  inspect(@azimuth.greet("Hello World"), "Hello, Hello World!")?
  
  // 测试数学运算的特殊情况
  inspect(@azimuth.add(0, 0), "0")?
  inspect(@azimuth.multiply(0, 0), "0")?
  inspect(@azimuth.multiply(1, 1), "1")?
  inspect(@azimuth.multiply(1, -1), "-1")?
}