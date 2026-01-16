// Azimuth 项目综合测试套件
// 使用标准 MoonBit 测试语法

test "add_function_comprehensive" {
  // 测试基本加法
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  
  // 测试负数加法
  @azimuth.assert_eq(-1, @azimuth.add(3, -4))
  
  // 测试零值加法
  @azimuth.assert_eq(10, @azimuth.add(10, 0))
  
  // 测试大数加法
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
}

test "multiply_function_comprehensive" {
  // 测试基本乘法
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  
  // 测试负数乘法
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  
  // 测试零值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
  
  // 测试大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "greet_function_comprehensive" {
  // 测试基本问候
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试数字字符串
  @azimuth.assert_eq_string("Hello, 2023!", @azimuth.greet("2023"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
}

test "mathematical_commutative_laws" {
  // 加法交换律测试
  let a = 42
  let b = 17
  @azimuth.assert_eq(
    @azimuth.add(a, b),
    @azimuth.add(b, a)
  )
  
  // 乘法交换律测试
  @azimuth.assert_eq(
    @azimuth.multiply(a, b),
    @azimuth.multiply(b, a)
  )
}

test "mathematical_associative_laws" {
  // 加法结合律测试
  let a = 10
  let b = 20
  let c = 30
  @azimuth.assert_eq(
    @azimuth.add(@azimuth.add(a, b), c),
    @azimuth.add(a, @azimuth.add(b, c))
  )
  
  // 乘法结合律测试
  @azimuth.assert_eq(
    @azimuth.multiply(@azimuth.multiply(a, b), c),
    @azimuth.multiply(a, @azimuth.multiply(b, c))
  )
}

test "boundary_value_tests" {
  // 测试最大值边界
  let max_int = 2147483647
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 0))
  
  // 测试最小值边界
  let min_int = -2147483648
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, 0))
  
  // 测试边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_int, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_int, 0))
}

test "complex_calculation_scenarios" {
  // 场景1：计算矩形面积
  let length = 10
  let width = 5
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(50, area)
  
  // 场景2：计算周长
  let perimeter = @azimuth.multiply(@azimuth.add(length, width), 2)
  @azimuth.assert_eq(30, perimeter)
  
  // 场景3：计算折扣价格
  let price = 100
  let discount = 20
  let discounted_price = @azimuth.add(price, -@azimuth.multiply(price, discount) / 100)
  @azimuth.assert_eq(80, discounted_price)
}

test "error_handling_and_edge_cases" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  // 测试相等断言
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试特殊字符处理
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
}

test "performance_and_stress_tests" {
  // 测试连续运算
  let result = @azimuth.add(
    @azimuth.multiply(100, 5),
    @azimuth.multiply(200, 3)
  )
  @azimuth.assert_eq(1100, result)
  
  // 测试嵌套运算
  let nested_result = @azimuth.multiply(
    @azimuth.add(10, 20),
    @azimuth.add(5, 5)
  )
  @azimuth.assert_eq(300, nested_result)
  
  // 测试大数连续运算
  let large_result = @azimuth.add(
    @azimuth.multiply(100000, 10),
    @azimuth.multiply(200000, 5)
  )
  @azimuth.assert_eq(2000000, large_result)
}

test "real_world_application_scenarios" {
  // 场景1：计算购物车总价
  let item1_price = 25
  let item2_price = 35
  let item3_price = 40
  let total = @azimuth.add(
    @azimuth.add(item1_price, item2_price),
    item3_price
  )
  @azimuth.assert_eq(100, total)
  
  // 场景2：计算平均分
  let score1 = 85
  let score2 = 90
  let score3 = 95
  let average = @azimuth.add(
    @azimuth.add(score1, score2),
    score3
  ) / 3
  @azimuth.assert_eq(90, average)
  
  // 场景3：计算温度转换（摄氏度到华氏度）
  let celsius = 25
  let fahrenheit = @azimuth.add(
    @azimuth.multiply(celsius, 9) / 5,
    32
  )
  @azimuth.assert_eq(77, fahrenheit)
}