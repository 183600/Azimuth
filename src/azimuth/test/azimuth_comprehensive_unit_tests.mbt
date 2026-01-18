// Azimuth 综合单元测试
// 包含10个标准测试用例，覆盖核心功能和边界情况

test "integer_arithmetic_basic" {
  // 基本整数算术运算测试
  let a = 42
  let b = 17
  
  // 加法测试
  let sum_result = @azimuth.add(a, b)
  @azimuth.assert_eq(59, sum_result)
  
  // 乘法测试
  let product_result = @azimuth.multiply(a, b)
  @azimuth.assert_eq(714, product_result)
  
  // 零值测试
  @azimuth.assert_eq(42, @azimuth.add(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
}

test "string_greeting_comprehensive" {
  // 字符串问候功能综合测试
  // 标准问候
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  
  // 空字符串处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单字符处理
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // Unicode字符处理
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 15
  let y = 25
  let z = 10
  
  // 加法交换律验证
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律验证
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 加法结合律验证
  @azimuth.assert_eq(
    @azimuth.add(@azimuth.add(x, y), z),
    @azimuth.add(x, @azimuth.add(y, z))
  )
  
  // 分配律验证
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, z)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  )
}

test "boundary_value_operations" {
  // 边界值运算测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加法溢出测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 最小值减法溢出测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 边界值乘法测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值相加测试
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "negative_number_calculations" {
  // 负数计算测试
  let neg_a = -20
  let neg_b = -15
  let pos_a = 20
  let pos_b = 15
  
  // 负数加法
  @azimuth.assert_eq(-35, @azimuth.add(neg_a, neg_b))
  
  // 负数乘法（负负得正）
  @azimuth.assert_eq(300, @azimuth.multiply(neg_a, neg_b))
  
  // 正负数混合运算
  @azimuth.assert_eq(5, @azimuth.add(neg_a, pos_b))
  @azimuth.assert_eq(-5, @azimuth.add(pos_a, neg_b))
  @azimuth.assert_eq(-300, @azimuth.multiply(neg_a, pos_b))
  @azimuth.assert_eq(-300, @azimuth.multiply(pos_a, neg_b))
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 圆形面积计算（简化版：π × r²，使用π≈3）
  let radius = 10
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(300, area)
  
  // 矩形周长计算
  let length = 15
  let width = 8
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(46, perimeter)
  
  // 平均值计算
  let num1 = 85
  let num2 = 90
  let num3 = 95
  let sum = @azimuth.add(@azimuth.add(num1, num2), num3)
  let average = sum / 3
  @azimuth.assert_eq(90, average)
}

test "real_world_applications" {
  // 实际应用场景测试
  // 购物车总价计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  let shipping = 20
  
  let subtotal1 = @azimuth.multiply(item1_price, item1_qty)
  let subtotal2 = @azimuth.multiply(item2_price, item2_qty)
  let total = @azimuth.add(@azimuth.add(subtotal1, subtotal2), shipping)
  
  @azimuth.assert_eq(717, total)
  
  // 温度转换（摄氏度转华氏度，简化版：F = C × 2 + 30）
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)
  
  // 简单利息计算
  let principal = 10000
  let rate = 5
  let years = 3
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total_amount = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total_amount)
}

test "edge_case_handling" {
  // 边界情况处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 特殊值处理
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
  
  // 大数运算
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
  
  // 极值附近的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  
  // 零值的各种运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
}

test "assert_functions_validation" {
  // 断言函数验证测试
  // 基本断言测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  // 数值相等断言
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq(-50, -50)
  @azimuth.assert_eq(0, 0)
  
  // 字符串相等断言
  @azimuth.assert_eq_string("Hello", "Hello")
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("Test123", "Test123")
  
  // 条件断言
  @azimuth.assert_true(@azimuth.add(2, 3) == 5)
  @azimuth.assert_true(@azimuth.multiply(4, 5) == 20)
  @azimuth.assert_false(@azimuth.add(2, 3) == 6)
  @azimuth.assert_false(@azimuth.multiply(4, 5) == 21)
}

test "sequential_operation_chains" {
  // 连续运算链测试
  // 复杂的连续加法
  let chain_sum = @azimuth.add(
    @azimuth.add(@azimuth.add(10, 20), 30),
    @azimuth.add(40, 50)
  )
  @azimuth.assert_eq(150, chain_sum)
  
  // 复杂的连续乘法
  let chain_product = @azimuth.multiply(
    @azimuth.multiply(@azimuth.multiply(2, 3), 4),
    @azimuth.multiply(5, 6)
  )
  @azimuth.assert_eq(720, chain_product)
  
  // 混合运算链
  let mixed_chain = @azimuth.add(
    @azimuth.multiply(10, 5),
    @azimuth.multiply(@azimuth.add(8, 2), 3)
  )
  @azimuth.assert_eq(80, mixed_chain)
  
  // 嵌套运算
  let nested = @azimuth.multiply(
    @azimuth.add(5, 3),
    @azimuth.add(@azimuth.multiply(2, 4), 1)
  )
  @azimuth.assert_eq(72, nested)
}