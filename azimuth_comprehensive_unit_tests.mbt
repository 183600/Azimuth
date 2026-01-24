// Azimuth 项目综合单元测试
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "add_function_comprehensive" {
  // 测试加法函数的综合情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

test "greet_function_unicode_and_special_chars" {
  // 测试问候函数的Unicode和特殊字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, Line\nBreak!", @azimuth.greet("Line\nBreak"))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的综合情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 装盒问题：47个物品，每盒12个，需要多少盒？
  let items = 47
  let box_capacity = 12
  let boxes_needed = @azimuth.divide_with_ceil(items, box_capacity)
  @azimuth.assert_eq(5, boxes_needed)
  
  // 价格计算：单价199，数量3，运费20
  let price = 199
  let quantity = 3
  let shipping = 20
  let subtotal = @azimuth.multiply(price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(617, total)
}

test "mathematical_properties_and_identities" {
  // 测试数学性质和恒等式
  let x = 42
  let y = 17
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  // 加法单位元
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  // 乘法单位元
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  // 乘法零元
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
}

test "error_handling_and_boundary_conditions" {
  // 测试错误处理和边界条件
  // 除数为零的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000000000, @azimuth.multiply(1000000, 1000000))
}

test "sequential_operations_pipeline" {
  // 测试连续操作管道
  let initial = 15
  let step1 = @azimuth.add(initial, 10)        // 25
  let step2 = @azimuth.multiply(step1, 2)      // 50
  let step3 = @azimuth.divide_with_ceil(step2, 12)  // 5
  let step4 = @azimuth.add(step3, 3)           // 8
  let step5 = @azimuth.multiply(step4, 1)      // 8
  
  @azimuth.assert_eq(8, step5)
}

test "string_processing_with_greet" {
  // 测试使用greet函数进行字符串处理
  let names = ["Alice", "Bob", "Charlie", "世界", "🚀"]
  let expected_greetings = [
    "Hello, Alice!",
    "Hello, Bob!", 
    "Hello, Charlie!",
    "Hello, 世界!",
    "Hello, 🚀!"
  ]
  
  @azimuth.assert_eq_string(expected_greetings[0], @azimuth.greet(names[0]))
  @azimuth.assert_eq_string(expected_greetings[1], @azimuth.greet(names[1]))
  @azimuth.assert_eq_string(expected_greetings[2], @azimuth.greet(names[2]))
  @azimuth.assert_eq_string(expected_greetings[3], @azimuth.greet(names[3]))
  @azimuth.assert_eq_string(expected_greetings[4], @azimuth.greet(names[4]))
}

test "real_world_inventory_management" {
  // 测试现实世界库存管理场景
  // 仓库有不同数量的商品，需要统一包装
  let product_a_qty = 23
  let product_b_qty = 47
  let product_c_qty = 89
  let package_size = 12
  
  let packages_a = @azimuth.divide_with_ceil(product_a_qty, package_size)
  let packages_b = @azimuth.divide_with_ceil(product_b_qty, package_size)
  let packages_c = @azimuth.divide_with_ceil(product_c_qty, package_size)
  
  let total_packages = @azimuth.add(@azimuth.add(packages_a, packages_b), packages_c)
  
  @azimuth.assert_eq(2, packages_a)  // ceil(23/12) = 2
  @azimuth.assert_eq(4, packages_b)  // ceil(47/12) = 4
  @azimuth.assert_eq(8, packages_c)  // ceil(89/12) = 8
  @azimuth.assert_eq(14, total_packages)  // 2 + 4 + 8 = 14
}