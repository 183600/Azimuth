// Azimuth 项目增强测试用例
// 使用标准 MoonBit 测试语法，提供全面的测试覆盖

test "greet_function_basic" {
  // 测试 greet 函数基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, Azimuth!", @azimuth.greet("Azimuth"))
}

test "greet_function_edge_cases" {
  // 测试 greet 函数边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
}

test "mathematical_operations_combination" {
  // 测试数学运算的组合
  
  // 复杂表达式: (a + b) * (c + d)
  let a = 5
  let b = 3
  let c = 2
  let d = 7
  
  let result = @azimuth.multiply(@azimuth.add(a, b), @azimuth.add(c, d))
  @azimuth.assert_eq(80, result)
  
  // 嵌套运算: a * (b + (c * d))
  let result2 = @azimuth.multiply(a, @azimuth.add(b, @azimuth.multiply(c, d)))
  @azimuth.assert_eq(85, result2)
}

test "calculator_simulation" {
  // 模拟简单计算器功能
  
  // 购物车计算: 商品1 * 数量1 + 商品2 * 数量2 + 商品3 * 数量3
  let price1 = 25
  let qty1 = 2
  let price2 = 15
  let qty2 = 3
  let price3 = 10
  let qty3 = 4
  
  let total = @azimuth.add(@azimuth.add(@azimuth.multiply(price1, qty1), @azimuth.multiply(price2, qty2)), @azimuth.multiply(price3, qty3))
  @azimuth.assert_eq(115, total)
  
  // 折扣计算: 总价 - 总价 * 折扣率
  let discount_rate = 10 // 10%
  let discounted_total = total - @azimuth.multiply(total, discount_rate) / 100
  @azimuth.assert_eq(103, discounted_total)
}

test "geometry_calculations" {
  // 几何计算测试
  
  // 长方形面积: 长 * 宽
  let length = 8
  let width = 6
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, rectangle_area)
  
  // 长方形周长: 2 * (长 + 宽)
  let rectangle_perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(28, rectangle_perimeter)
  
  // 三角形面积: 底 * 高 / 2
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "number_theory_properties" {
  // 数论性质测试
  
  // 奇偶性测试
  let even_num = 8
  let odd_num = 7
  
  // 偶数 + 偶数 = 偶数
  @azimuth.assert_eq(0, @azimuth.add(even_num, even_num) % 2)
  
  // 奇数 + 奇数 = 偶数
  @azimuth.assert_eq(0, @azimuth.add(odd_num, odd_num) % 2)
  
  // 奇数 + 偶数 = 奇数
  @azimuth.assert_eq(1, @azimuth.add(odd_num, even_num) % 2)
  
  // 乘法奇偶性
  // 偶数 * 任何数 = 偶数
  @azimuth.assert_eq(0, @azimuth.multiply(even_num, odd_num) % 2)
  @azimuth.assert_eq(0, @azimuth.multiply(even_num, even_num) % 2)
  
  // 奇数 * 奇数 = 奇数
  @azimuth.assert_eq(1, @azimuth.multiply(odd_num, odd_num) % 2)
}

test "time_and_date_calculations" {
  // 时间和日期计算测试
  
  // 计算一周的总分钟数
  let days_in_week = 7
  let hours_in_day = 24
  let minutes_in_hour = 60
  let minutes_in_week = @azimuth.multiply(days_in_week, @azimuth.multiply(hours_in_day, minutes_in_hour))
  @azimuth.assert_eq(10080, minutes_in_week)
  
  // 计算一年的总天数（不考虑闰年）
  let months_in_year = 12
  let avg_days_in_month = 30
  let days_in_year = @azimuth.multiply(months_in_year, avg_days_in_month)
  @azimuth.assert_eq(360, days_in_year)
}

test "physics_simulations" {
  // 物理模拟计算测试
  
  // 速度计算: 距离 / 时间
  let distance = 120 // 米
  let time = 10 // 秒
  let speed = distance / time
  @azimuth.assert_eq(12, speed)
  
  // 动能计算: 0.5 * 质量 * 速度² (使用整数近似)
  let mass = 10 // 千克
  let velocity = 5 // 米/秒
  let kinetic_energy = @azimuth.multiply(mass, @azimuth.multiply(velocity, velocity)) / 2
  @azimuth.assert_eq(125, kinetic_energy)
  
  // 势能计算: 质量 * 重力加速度 * 高度
  let height = 20 // 米
  let gravity = 10 // 米/秒² (简化值)
  let potential_energy = @azimuth.multiply(mass, @azimuth.multiply(gravity, height))
  @azimuth.assert_eq(2000, potential_energy)
}

test "statistical_calculations" {
  // 统计计算测试
  
  // 计算数组平均值
  let values = [15, 25, 35, 45, 55]
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(values[0], values[1]), values[2]), values[3]), values[4])
  let count = 5
  let average = sum / count
  @azimuth.assert_eq(35, average)
  
  // 计算中位数（奇数个元素）
  @azimuth.assert_eq(35, values[2]) // 已排序数组的中位数
  
  // 计算范围（最大值 - 最小值）
  let min_value = values[0]
  let max_value = values[4]
  let range = max_value - min_value
  @azimuth.assert_eq(40, range)
}

test "performance_large_numbers" {
  // 大数性能测试
  
  // 大数加法
  let large_num1 = 999999
  let large_num2 = 888888
  let large_sum = @azimuth.add(large_num1, large_num2)
  @azimuth.assert_eq(1888887, large_sum)
  
  // 大数乘法
  let large_num3 = 12345
  let large_num4 = 6789
  let large_product = @azimuth.multiply(large_num3, large_num4)
  @azimuth.assert_eq(83810205, large_product)
  
  // 连续运算性能测试
  let result = large_num1
  let iterations = 10
  for i = 0; i < iterations; i = i + 1 {
    result = @azimuth.add(result, large_num2)
  }
  @azimuth.assert_eq(@azimuth.add(large_num1, @azimuth.multiply(large_num2, iterations)), result)
}