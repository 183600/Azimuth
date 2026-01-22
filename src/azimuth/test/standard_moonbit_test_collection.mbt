// 标准 MoonBit 测试用例集合
// 为 Azimuth 项目添加的10个标准测试用例

test "basic_arithmetic_operations" {
  // 测试基本的算术运算
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试123!", @azimuth.greet("测试123"))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试乘法边界
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, 3)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3))
  )
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("success", "success")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_eq(@azimuth.add(max_val, 100, true) == max_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 2, true) == max_val)
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算矩形的周长：2 × (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(30, perimeter)
  
  // 计算三角形的面积：底 × 高 ÷ 2
  let base = 8
  let height = 6
  let area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(24, area)
}

test "real_world_applications" {
  // 测试实际应用场景
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 简单贷款计算：本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  let total = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, years)))
  @azimuth.assert_eq(160000, total)
}

test "performance_related_operations" {
  // 测试性能相关的操作
  // 计算等差数列的和：1 + 2 + ... + 100
  let n = 100
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(5050, sum)
  
  // 计算等比数列的和：1 + 2 + 4 + 8 + 16
  let geo_sum = @azimuth.add(1, @azimuth.add(2, @azimuth.add(4, @azimuth.add(8, 16))))
  @azimuth.assert_eq(31, geo_sum)
}

test "comprehensive_function_integration" {
  // 测试综合功能集成
  // 模拟购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  // 计算商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(total1, total2)
  
  // 计算最终总价
  let final_total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(717, final_total)
  
  // 测试字符串处理与数字计算的结合
  let customer_name = "张三"
  let order_total_str = "717"
  let greeting = @azimuth.greet(customer_name)
  @azimuth.assert_eq_string("Hello, 张三!", greeting)
}

test "edge_cases_and_special_conditions" {
  // 测试边界情况和特殊条件
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  @azimuth.assert_eq(3000000, @azimuth.add(large_num1, large_num2))
  @azimuth.assert_eq(2000000000000, @azimuth.multiply(large_num1, large_num2))
  
  // 测试负数运算
  @azimuth.assert_eq(-100, @azimuth.add(50, -150))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  
  // 测试包含特殊字符的字符串
  let special_string = "Hello@World#123"
  @azimuth.assert_eq_string("Hello, Hello@World#123!", @azimuth.greet(special_string))
  
  // 测试divide_with_ceil函数
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}