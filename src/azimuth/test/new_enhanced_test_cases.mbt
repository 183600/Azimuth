// 新增的增强测试用例，使用标准 MoonBit 测试语法
test "enhanced_arithmetic_operations" {
  // 测试增强的算术运算
  // 复合运算：(a + b) * (a - b)
  let sum = @azimuth.add(15, 7)
  let diff = @azimuth.add(15, -7)
  let result = @azimuth.multiply(sum, diff)
  
  @azimuth.assert_eq(176, result)  // (15+7)*(15-7) = 22*8 = 176
}

test "enhanced_string_operations" {
  // 测试增强的字符串操作
  let result1 = @azimuth.greet("MoonBit")
  let result2 = @azimuth.greet("开发者")
  let result3 = @azimuth.greet("123")
  
  @azimuth.assert_eq_string("Hello, MoonBit!", result1)
  @azimuth.assert_eq_string("Hello, 开发者!", result2)
  @azimuth.assert_eq_string("Hello, 123!", result3)
}

test "enhanced_boundary_value_testing" {
  // 测试增强的边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法测试
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "enhanced_mathematical_formulas" {
  // 测试增强的数学公式
  // 计算二次多项式：ax² + bx + c，其中 a=2, b=-3, c=7
  let x_squared = @azimuth.multiply(12, 12)
  let ax_squared = @azimuth.multiply(2, x_squared)
  let bx = @azimuth.multiply(-3, 12)
  let polynomial_result = @azimuth.add(@azimuth.add(ax_squared, bx), 7)
  
  @azimuth.assert_eq(259, polynomial_result)  // 2*144 + (-3)*12 + 7 = 288 - 36 + 7 = 259
}

test "enhanced_business_calculations" {
  // 测试增强的业务计算
  let unit_price = 199
  let quantity = 5
  let discount_rate = 15  // 15% 折扣
  let tax_rate = 8        // 8% 税率
  
  // 计算总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算折扣
  let discount_amount = @azimuth.multiply(subtotal, discount_rate) / 100
  let discounted_price = @azimuth.add(subtotal, -discount_amount)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_rate) / 100
  
  // 计算最终价格
  let final_price = @azimuth.add(discounted_price, tax_amount)
  
  @azimuth.assert_eq(912, final_price)  // 199*5 = 995, 995*0.85 = 845, 845*0.08 = 67, 845+67 = 912
}

test "enhanced_scientific_calculations" {
  // 测试增强的科学计算
  // 简化的物理公式：距离 = 速度 × 时间
  let initial_velocity = 10
  let acceleration = 5
  let time = 8
  
  // 计算末速度：v = u + at
  let final_velocity = @azimuth.add(initial_velocity, @azimuth.multiply(acceleration, time))
  
  // 计算平均速度：(u + v) / 2
  let average_velocity = @azimuth.add(initial_velocity, final_velocity) / 2
  
  // 计算距离：s = 平均速度 × 时间
  let distance = @azimuth.multiply(average_velocity, time)
  
  @azimuth.assert_eq(240, distance)  // v = 10 + 5*8 = 50, avg = (10+50)/2 = 30, s = 30*8 = 240
}

test "enhanced_error_handling" {
  // 测试增强的错误处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正常情况测试
  @azimuth.assert_true(@azimuth.add(10, 20) == 30)
  @azimuth.assert_false(@azimuth.add(10, 20) == 40)
  @azimuth.assert_eq(30, @azimuth.add(10, 20))
  
  // 边界情况测试
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  
  // 字符串测试
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
}

test "enhanced_data_structure_operations" {
  // 测试增强的数据结构操作（模拟）
  // 模拟数组索引计算
  // 二维数组转一维数组的索引
  let index_1d = @azimuth.add(@azimuth.multiply(3, 10), 4)
  @azimuth.assert_eq(34, index_1d)  // 3*10 + 4 = 34
  
  // 模拟循环边界检查
  let array_size = 100
  let index = 50
  let is_valid = @azimuth.add(index, 1) <= array_size
  @azimuth.assert_true(is_valid)
}

test "enhanced_algorithm_testing" {
  // 测试增强的算法
  // 模拟二分查找的中间值计算
  let mid = @azimuth.add(0, 99) / 2
  @azimuth.assert_eq(49, mid)
  
  // 模拟斐波那契数列计算（前几项）
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
}

test "enhanced_performance_testing" {
  // 测试增强的性能测试
  // 模拟大数据量计算
  let large_result = @azimuth.multiply(1000, 1000)
  @azimuth.assert_eq(1000000, large_result)
  
  // 模拟累加计算：1 + 2 + ... + 100 = 100 * 101 / 2
  let n = 100
  let sum_formula = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(5050, sum_formula)
  
  // 模拟幂运算：2^8 = 256
  let power_of_2 = @azimuth.multiply(
    @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)),
    @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2))
  )
  @azimuth.assert_eq(256, power_of_2)  // 2^8 = 256
}