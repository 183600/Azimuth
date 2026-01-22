// 新的高质量 MoonBit 测试用例
// 覆盖边界条件、错误处理和实际应用场景

test "add_function_identity_property" {
  // 测试加法的单位元性质
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "multiply_function_distributive_property" {
  // 测试乘法对加法的分配律 a * (b + c) = a * b + a * c
  let a = 5
  let b = 3
  let c = 2
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
  
  // 测试另一组数值
  let x = 10
  let y = 7
  let z = 4
  
  let left_side2 = @azimuth.multiply(x, @azimuth.add(y, z))
  let right_side2 = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  
  @azimuth.assert_eq(left_side2, right_side2)
}

test "divide_with_ceil_practical_scenarios" {
  // 测试实际应用场景中的向上取整除法
  
  // 场景1：计算需要的容器数量
  let items = 25
  let container_capacity = 6
  let containers_needed = @azimuth.divide_with_ceil(items, container_capacity)
  @azimuth.assert_eq(5, containers_needed)  // 25个物品，每个容器装6个，需要5个容器
  
  // 场景2：计算需要的页数
  let total_records = 100
  let records_per_page = 15
  let pages_needed = @azimuth.divide_with_ceil(total_records, records_per_page)
  @azimuth.assert_eq(7, pages_needed)  // 100条记录，每页15条，需要7页
  
  // 场景3：计算需要的车辆数量
  let passengers = 58
  let vehicle_capacity = 12
  let vehicles_needed = @azimuth.divide_with_ceil(passengers, vehicle_capacity)
  @azimuth.assert_eq(5, vehicles_needed)  // 58名乘客，每辆车坐12人，需要5辆车
}

test "complex_business_logic" {
  // 测试复杂的业务逻辑计算
  
  // 计算订单总价：商品价格 × 数量 + 运费
  let unit_price = 45
  let quantity = 8
  let shipping = 12
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(372, total)  // 45 * 8 + 12 = 360 + 12 = 372
  
  // 计算折扣价格：原价 - 折扣金额
  let original_price = 500
  let discount_percent = 20  // 20% 折扣
  let discount_amount = @azimuth.divide_with_ceil(@azimuth.multiply(original_price, discount_percent), 100)
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(400, discounted_price)  // 500 - (500 * 20 / 100) = 500 - 100 = 400
}

test "greet_function_multilingual_support" {
  // 测试多语言支持的问候功能
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Mundo!", @azimuth.greet("Mundo"))
  @azimuth.assert_eq_string("Hello, Monde!", @azimuth.greet("Monde"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 👋!", @azimuth.greet("👋"))
  @azimuth.assert_eq_string("Hello, Ñoño!", @azimuth.greet("Ñoño"))
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  
  // 除数为零的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 0))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_int, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_int, 0))
}

test "performance_and_stress_testing" {
  // 测试性能和压力情况
  
  // 连续运算测试
  let mut result = 1
  result = @azimuth.add(result, 1)  // 2
  result = @azimuth.multiply(result, 2)  // 4
  result = @azimuth.add(result, 1)  // 5
  result = @azimuth.multiply(result, 2)  // 10
  result = @azimuth.add(result, 1)  // 11
  result = @azimuth.multiply(result, 2)  // 22
  
  @azimuth.assert_eq(22, result)
  
  // 嵌套运算测试
  let nested_result = @azimuth.add(
    @azimuth.multiply(2, 3),
    @azimuth.multiply(4, @azimuth.add(1, 2))
  )
  @azimuth.assert_eq(18, nested_result)  // 2*3 + 4*(1+2) = 6 + 12 = 18
}

test "mathematical_properties_commutativity" {
  // 测试数学交换律性质
  
  // 加法交换律：a + b = b + a
  let a = 123
  let b = 456
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律：a * b = b * a
  let x = 17
  let y = 29
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 测试负数的交换律
  let neg_a = -15
  let neg_b = 25
  @azimuth.assert_eq(@azimuth.add(neg_a, neg_b), @azimuth.add(neg_b, neg_a))
  @azimuth.assert_eq(@azimuth.multiply(neg_a, neg_b), @azimuth.multiply(neg_b, neg_a))
}

test "divide_with_ceil_boundary_analysis" {
  // 测试向上取整除法的边界情况分析
  
  // 完全整除的情况
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(25, 5))
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(49, 7))
  
  // 接近整除的情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(13, 4))  // 13/4 = 3.25 -> 4
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(14, 4))  // 14/4 = 3.5 -> 4
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(15, 4))  // 15/4 = 3.75 -> 4
  
  // 负数边界情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-12, 4))  // -12/4 = -3 -> -3
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-11, 4))  // -11/4 = -2.75 -> -2
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-10, 4))  // -10/4 = -2.5 -> -2
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-9, 4))   // -9/4 = -2.25 -> -2
}

test "string_processing_comprehensive" {
  // 测试字符串处理的综合情况
  
  // 空字符串处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 特殊字符处理
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, '\"!", @azimuth.greet("'\""))
  
  // 长字符串处理
  let long_name = "VeryLongNameThatExceedsNormalExpectations"
  let expected = "Hello, " + long_name + "!"
  @azimuth.assert_eq_string(expected, @azimuth.greet(long_name))
  
  // 数字字符串处理
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, 3.14159!", @azimuth.greet("3.14159"))
}