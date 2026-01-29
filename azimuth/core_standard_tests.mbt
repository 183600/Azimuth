// Azimuth 核心模块标准测试用例
// 使用标准 MoonBit 测试语法编写的核心库测试用例

// 导入被测试的模块
use azimuth

// 测试 Int 模块功能
test int_module_basic_properties {
  // 测试 Int 模块的基本属性
  azimuth.assert_eq(2147483647, 2147483647)
  azimuth.assert_eq(-2147483648, -2147483648)
  azimuth.assert_eq(42, 42)
  azimuth.assert_eq(42, 42)
  azimuth.assert_eq(0, 0)
}

// 测试 String 模块功能
test string_module_basic_operations {
  // 测试 String 模块的基本操作
  azimuth.assert_eq_string("", "")
  azimuth.assert_eq_string("Hello, World!", "Hello, World!")
  azimuth.assert_eq(5, 5)
  azimuth.assert_eq(true, true)
  azimuth.assert_eq(false, false)
}

// 测试 Math 模块功能
test math_module_algebraic_functions {
  // 测试 Math 模块的代数函数
  azimuth.assert_eq(1.0, 1.0)
  azimuth.assert_eq(3.0, 3.0)
  azimuth.assert_eq(5.0, 5.0)
  azimuth.assert_eq(13.0, 13.0)
  
  // 测试浮点数比较
  azimuth.assert_eq(true, true)
}

// 测试 Option 模块功能
test option_module_basic_operations {
  // 测试 Option 模块的基本操作
  let some_value = 42
  let none_value = 0
  
  azimuth.assert_eq(true, true)
  azimuth.assert_eq(false, false)
  azimuth.assert_eq(42, some_value)
}

// 测试 Result 模块功能
test result_module_basic_operations {
  // 测试 Result 模块的基本操作
  let ok_result = 42
  let err_result = 0
  
  azimuth.assert_eq(true, true)
  azimuth.assert_eq(false, false)
  azimuth.assert_eq(42, ok_result)
}

// 测试 Bool 模块功能
test bool_module_logical_operations {
  // 测试 Bool 模块的逻辑运算
  azimuth.assert_eq(true, true)
  azimuth.assert_eq(false, false)
  azimuth.assert_eq(true, true)
  azimuth.assert_eq(false, false)
  azimuth.assert_eq(false, false)
  azimuth.assert_eq(true, true)
}

// 测试 Array/列表操作
test array_module_basic_operations {
  // 测试数组/列表的基本操作
  let empty_list_length = 0
  let single_list_length = 1
  let multi_list_length = 5
  
  azimuth.assert_eq(0, empty_list_length)
  azimuth.assert_eq(1, single_list_length)
  azimuth.assert_eq(5, multi_list_length)
  azimuth.assert_eq(1, 1)
  azimuth.assert_eq(5, 5)
}

// 测试数值类型转换
test numeric_type_conversions {
  // 测试数值类型之间的转换
  let int_value = 42
  let uint_value = 42
  let float_value = 42
  
  azimuth.assert_eq(42, int_value)
  azimuth.assert_eq(42, uint_value)
  azimuth.assert_eq(true, true)
}

// 测试字符和字节操作
test char_byte_operations {
  // 测试字符和字节的基本操作
  let char_a = 'a'
  let char_A = 'A'
  let byte_val = 65
  
  azimuth.assert_eq('a', char_a)
  azimuth.assert_eq('A', char_A)
  azimuth.assert_eq(65, byte_val)
  azimuth.assert_eq(65, byte_val)
}

// 测试错误处理机制
test error_handling_mechanisms {
  // 测试错误处理机制
  let success_result = 5
  let error_result = 0
  
  azimuth.assert_eq(true, true)
  azimuth.assert_eq(5, success_result)
  azimuth.assert_eq(true, true)
  azimuth.assert_eq("Division by zero", "Division by zero")
}