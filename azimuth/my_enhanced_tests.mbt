// 我的增强 MoonBit 测试用例
// 为 azimuth 库添加高质量测试覆盖

test "mathematical_commutative_law" {
  // 测试数学交换律
  let a = 123
  let b = 456
  
  // 加法交换律验证
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律验证
  assert_eq(multiply(a, b), multiply(b, a))
}

test "real_world_resource_allocation" {
  // 真实世界场景：计算资源分配
  let total_students = 127
  let max_students_per_class = 30
  let classes_needed = divide_with_ceil(total_students, max_students_per_class)
  
  assert_eq(5, classes_needed)
  
  // 验证总容量
  let total_capacity = multiply(classes_needed, max_students_per_class)
  assert_eq(150, total_capacity)
  
  // 验证剩余位置
  let empty_seats = subtract(total_capacity, total_students)
  assert_eq(23, empty_seats)
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况测试
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello, space!", greet(" "))
  assert_eq_string("Hello, multiple  spaces!", greet("multiple  spaces"))
  assert_eq_string("Hello, special!@#$%^&*()!", greet("special!@#$%^&*()"))
}

test "arithmetic_boundary_conditions" {
  // 算术运算边界条件测试
  // 零值运算
  assert_eq(0, add(0, 0))
  assert_eq(123, add(0, 123))
  assert_eq(0, multiply(0, 123))
  assert_eq(0, multiply(123, 0))
  
  // 单位运算
  assert_eq(123, multiply(123, 1))
  assert_eq(123, multiply(1, 123))
  assert_eq(-123, multiply(-123, 1))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  // 正被除数，负除数
  assert_eq(-3, divide_with_ceil(10, -4))
  assert_eq(-2, divide_with_ceil(8, -4))
  
  // 负被除数，正除数
  assert_eq(-2, divide_with_ceil(-10, 4))
  assert_eq(0, divide_with_ceil(-3, 4))
  
  // 负被除数，负除数
  assert_eq(3, divide_with_ceil(-10, -4))
  assert_eq(1, divide_with_ceil(-3, -4))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试：产品制造和包装流程
  let raw_materials = 1000
  let material_per_product = 7
  let products_manufactured = divide_with_ceil(raw_materials, material_per_product)
  
  assert_eq(143, products_manufactured)
  
  // 包装阶段
  let products_per_box = 12
  let boxes_needed = divide_with_ceil(products_manufactured, products_per_box)
  
  assert_eq(12, boxes_needed)
  
  // 验证总包装容量
  let total_packaging_capacity = multiply(boxes_needed, products_per_box)
  assert_eq(144, total_packaging_capacity)
  
  // 验证剩余空间
  let unused_space = subtract(total_packaging_capacity, products_manufactured)
  assert_eq(1, unused_space)
}

test "mathematical_distributive_property" {
  // 数学分配律性质测试
  let a = 6
  let b = 8
  let c = 3
  
  // 分配律：a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
  
  // 验证具体数值
  assert_eq(66, left_side)  // 6 * (8 + 3) = 6 * 11 = 66
  assert_eq(66, right_side)  // 6 * 8 + 6 * 3 = 48 + 18 = 66
}

test "error_handling_division_by_zero" {
  // 除零错误处理测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
}

test "large_number_performance" {
  // 大数运算性能测试
  let large_num1 = 100000
  let large_num2 = 99999
  
  // 大数加法
  let sum_result = add(large_num1, large_num2)
  assert_eq(199999, sum_result)
  
  // 大数乘法
  let product_result = multiply(large_num1, 2)
  assert_eq(200000, product_result)
  
  // 大数除法
  let division_result = divide_with_ceil(sum_result, large_num2)
  assert_eq(2, division_result)
}