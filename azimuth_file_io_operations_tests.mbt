// Azimuth File I/O Operations Test Suite
// 文件I/O操作测试套件 - 专注于文件读写、流处理和存储管理

// 测试1: 基本文件读写操作
test "基本文件读写操作测试" {
  // 创建临时文件路径
  let temp_file_path = "/tmp/azimuth_telemetry_test.txt"
  
  // 创建文件写入器
  let file_writer = FileWriter::new()
  FileWriter::set_buffer_size(file_writer, 8192) // 8KB缓冲区
  FileWriter::set_encoding(file_writer, FileEncoding::UTF8)
  FileWriter::set_line_ending(file_writer, LineEnding::Unix)
  
  // 写入遥测数据
  let telemetry_lines = [
    "trace_id,span_id,service_name,operation_name,start_time,end_time,status",
    "trace-001,span-001,azimuth-telemetry,http.request,1640995200000,1640995200500,ok",
    "trace-002,span-002,azimuth-telemetry,db.query,1640995200600,1640995200800,ok",
    "trace-003,span-003,azimuth-telemetry,cache.get,1640995200900,1640995200950,error",
    "trace-004,span-004,azimuth-telemetry,api.call,1640995201000,1640995201200,ok"
  ]
  
  // 测试文件写入
  let write_start_time = Clock::now_unix_nanos(Clock::system())
  let write_result = FileWriter::write_lines(file_writer, temp_file_path, telemetry_lines)
  let write_end_time = Clock::now_unix_nanos(Clock::system())
  
  assert_true(write_result.success)
  assert_true(write_result.bytes_written > 0)
  assert_eq(write_result.lines_written, 5)
  
  // 验证写入性能
  let write_duration = write_end_time - write_start_time
  assert_true(write_duration < 100000000) // 小于100ms
  
  // 验证文件存在
  assert_true(FileSystem::file_exists(temp_file_path))
  
  // 验证文件大小
  let file_size = FileSystem::get_file_size(temp_file_path)
  assert_true(file_size > 0)
  
  // 创建文件读取器
  let file_reader = FileReader::new()
  FileReader::set_buffer_size(file_reader, 8192) // 8KB缓冲区
  FileReader::set_encoding(file_reader, FileEncoding::UTF8)
  
  // 测试文件读取
  let read_start_time = Clock::now_unix_nanos(Clock::system())
  let read_result = FileReader::read_lines(file_reader, temp_file_path)
  let read_end_time = Clock::now_unix_nanos(Clock::system())
  
  assert_true(read_result.success)
  assert_eq(read_result.lines.length(), 5)
  assert_true(read_result.bytes_read > 0)
  
  // 验证读取性能
  let read_duration = read_end_time - read_start_time
  assert_true(read_duration < 100000000) // 小于100ms
  
  // 验证内容完整性
  assert_eq(read_result.lines[0], "trace_id,span_id,service_name,operation_name,start_time,end_time,status")
  assert_eq(read_result.lines[1], "trace-001,span-001,azimuth-telemetry,http.request,1640995200000,1640995200500,ok")
  assert_eq(read_result.lines[4], "trace-004,span-004,azimuth-telemetry,api.call,1640995201000,1640995201200,ok")
  
  // 清理临时文件
  let delete_result = FileSystem::delete_file(temp_file_path)
  assert_true(delete_result.success)
  assert_false(FileSystem::file_exists(temp_file_path))
}

// 测试2: 二进制文件操作
test "二进制文件操作测试" {
  // 创建二进制文件路径
  let binary_file_path = "/tmp/azimuth_binary_test.bin"
  
  // 创建二进制数据
  let telemetry_header = BinaryData::from_string("AZIMUTH")
  let version = BinaryData::from_bytes([0x01, 0x00]) // 版本1.0
  let record_count = BinaryData::from_bytes([0x05, 0x00, 0x00, 0x00]) // 5条记录
  
  // 创建遥测记录
  let telemetry_records = []
  for i in 1..=5 {
    let trace_id = BinaryData::from_string("trace-00" + i.to_string())
    let span_id = BinaryData::from_string("span-00" + i.to_string())
    let timestamp = BinaryData::from_bytes([
      0x00, 0x00, 0x00, 0x00, 
      (1640995200000 >> 24) & 0xFF,
      (1640995200000 >> 16) & 0xFF,
      (1640995200000 >> 8) & 0xFF,
      1640995200000 & 0xFF
    ])
    let status = BinaryData::from_bytes([if i == 3 { 0x01 } else { 0x00 }]) // 第3条记录为错误状态
    
    let record = BinaryData::concat([trace_id, span_id, timestamp, status])
    telemetry_records.push(record)
  }
  
  // 创建完整的二进制数据
  let binary_data = BinaryData::concat([
    telemetry_header,
    version,
    record_count,
    BinaryData::concat(telemetry_records)
  ])
  
  // 写入二进制文件
  let binary_writer = BinaryWriter::new()
  binary_writer.set_endian(BinaryEndian::Little)
  
  let write_result = BinaryWriter::write_all(binary_writer, binary_file_path, binary_data)
  assert_true(write_result.success)
  assert_eq(write_result.bytes_written, binary_data.length())
  
  // 读取二进制文件
  let binary_reader = BinaryReader::new()
  binary_reader.set_endian(BinaryEndian::Little)
  
  let read_result = BinaryReader::read_all(binary_reader, binary_file_path)
  assert_true(read_result.success)
  assert_eq(read_result.data.length(), binary_data.length())
  
  // 验证二进制数据
  let read_data = read_result.data
  assert_true(read_data.starts_with(BinaryData::from_string("AZIMUTH")))
  
  // 解析二进制数据
  let header = read_data.slice(0, 7)
  let version_bytes = read_data.slice(7, 2)
  let count_bytes = read_data.slice(9, 4)
  
  assert_eq(header, BinaryData::from_string("AZIMUTH"))
  assert_eq(version_bytes, BinaryData::from_bytes([0x01, 0x00]))
  assert_eq(count_bytes, BinaryData::from_bytes([0x05, 0x00, 0x00, 0x00]))
  
  // 测试随机访问
  let random_reader = RandomAccessReader::new(binary_file_path)
  
  // 读取版本信息
  let version_result = RandomAccessReader::read_bytes(random_reader, 7, 2)
  assert_true(version_result.success)
  assert_eq(version_result.data, BinaryData::from_bytes([0x01, 0x00]))
  
  // 读取第一条记录
  let first_record_start = 13
  let first_record_length = 22 // 假设每条记录22字节
  let first_record_result = RandomAccessReader::read_bytes(random_reader, first_record_start, first_record_length)
  assert_true(first_record_result.success)
  assert_eq(first_record_result.data.length(), first_record_length)
  
  // 清理临时文件
  FileSystem::delete_file(binary_file_path)
}

// 测试3: 文件流处理
test "文件流处理测试" {
  // 创建大型数据文件
  let large_file_path = "/tmp/azimuth_large_data.csv"
  
  // 创建文件流写入器
  let stream_writer = FileStreamWriter::new(large_file_path)
  stream_writer.set_buffer_size(16384) // 16KB缓冲区
  stream_writer.set_auto_flush(true)
  
  // 写入CSV头部
  let csv_header = "trace_id,span_id,service_name,operation_name,duration_ms,status_code\n"
  let header_result = stream_writer.write_string(csv_header)
  assert_true(header_result.success)
  
  // 流式写入大量遥测数据
  let write_start_time = Clock::now_unix_nanos(Clock::system())
  
  for i in 1..=10000 {
    let trace_id = "trace-" + String::pad_left(i.to_string(), 6, "0")
    let span_id = "span-" + String::pad_left(i.to_string(), 6, "0")
    let service_name = "azimuth-telemetry"
    let operation_name = ["http.request", "db.query", "cache.get", "api.call", "file.read"][i % 5]
    let duration_ms = 50 + (i % 500)
    let status_code = if i % 20 == 0 { "500" } else { "200" }
    
    let csv_line = trace_id + "," + span_id + "," + service_name + "," + 
                  operation_name + "," + duration_ms.to_string() + "," + status_code + "\n"
    
    let line_result = stream_writer.write_string(csv_line)
    assert_true(line_result.success)
  }
  
  // 关闭流写入器
  let close_result = stream_writer.close()
  assert_true(close_result.success)
  
  let write_end_time = Clock::now_unix_nanos(Clock::system())
  let write_duration = write_end_time - write_start_time
  
  // 验证写入性能
  assert_true(write_duration < 5000000000) // 小于5秒
  
  // 验证文件大小
  let file_size = FileSystem::get_file_size(large_file_path)
  assert_true(file_size > 1000000) // 大于1MB
  
  // 创建文件流读取器
  let stream_reader = FileStreamReader::new(large_file_path)
  stream_reader.set_buffer_size(16384) // 16KB缓冲区
  
  // 读取CSV头部
  let header_line_result = stream_reader.read_line()
  assert_true(header_line_result.success)
  assert_eq(header_line_result.line, "trace_id,span_id,service_name,operation_name,duration_ms,status_code")
  
  // 流式读取和处理数据
  let read_start_time = Clock::now_unix_nanos(Clock::system())
  let mut line_count = 0
  let mut error_count = 0
  let mut total_duration = 0
  
  while true {
    let line_result = stream_reader.read_line()
    if not(line_result.success) {
      break
    }
    
    let line = line_result.line
    let fields = line.split(",")
    
    if fields.length() == 6 {
      line_count = line_count + 1
      total_duration = total_duration + fields[4].to_int()
      
      if fields[5] == "500" {
        error_count = error_count + 1
      }
    }
  }
  
  let read_end_time = Clock::now_unix_nanos(Clock::system())
  let read_duration = read_end_time - read_start_time
  
  // 验证读取结果
  assert_eq(line_count, 10000)
  assert_eq(error_count, 500) // 每20条记录中有1条错误
  assert_true(total_duration > 0)
  
  // 验证读取性能
  assert_true(read_duration < 3000000000) // 小于3秒
  
  // 计算平均持续时间
  let avg_duration = total_duration / line_count
  assert_true(avg_duration >= 50 && avg_duration <= 549) // 50 + (0-499)的平均值
  
  // 关闭流读取器
  let reader_close_result = stream_reader.close()
  assert_true(reader_close_result.success)
  
  // 清理临时文件
  FileSystem::delete_file(large_file_path)
}

// 测试4: 文件监控和变更检测
test "文件监控和变更检测测试" {
  // 创建监控目录
  let monitor_dir = "/tmp/azimuth_monitor_test"
  FileSystem::create_directory(monitor_dir)
  
  // 创建文件监控器
  let file_monitor = FileMonitor::new()
  FileMonitor::add_watch_directory(file_monitor, monitor_dir)
  FileMonitor::set_watch_mode(file_monitor, WatchMode::Recursive) // 递归监控
  FileMonitor::set_event_filter(file_monitor, [
    FileEventType::Created,
    FileEventType::Modified,
    FileEventType::Deleted,
    FileEventType::Moved
  ])
  
  // 启动监控
  let start_result = FileMonitor::start(file_monitor)
  assert_true(start_result.success)
  
  // 创建测试文件
  let test_file1 = monitor_dir + "/test1.txt"
  let test_file2 = monitor_dir + "/subdir/test2.txt"
  
  // 创建子目录
  FileSystem::create_directory(monitor_dir + "/subdir")
  
  // 测试文件创建事件
  let create_result = FileSystem::write_file(test_file1, "initial content")
  assert_true(create_result.success)
  
  // 等待事件处理
  Thread::sleep(100)
  
  // 检查创建事件
  let create_events = FileMonitor::get_events(file_monitor, FileEventType::Created)
  assert_true(create_events.length() >= 1)
  
  let test1_created = create_events.some(fn(event) {
    event.file_path == test_file1
  })
  assert_true(test1_created)
  
  // 测试文件修改事件
  let modify_result = FileSystem::append_file(test_file1, "\nmodified content")
  assert_true(modify_result.success)
  
  // 等待事件处理
  Thread::sleep(100)
  
  // 检查修改事件
  let modify_events = FileMonitor::get_events(file_monitor, FileEventType::Modified)
  assert_true(modify_events.length() >= 1)
  
  let test1_modified = modify_events.some(fn(event) {
    event.file_path == test_file1
  })
  assert_true(test1_modified)
  
  // 测试子目录中的文件创建
  let subdir_create_result = FileSystem::write_file(test_file2, "subdir content")
  assert_true(subdir_create_result.success)
  
  // 等待事件处理
  Thread::sleep(100)
  
  // 检查子目录文件创建事件
  let subdir_create_events = FileMonitor::get_events(file_monitor, FileEventType::Created)
  let test2_created = subdir_create_events.some(fn(event) {
    event.file_path == test_file2
  })
  assert_true(test2_created)
  
  // 测试文件移动/重命名
  let renamed_file = monitor_dir + "/test1_renamed.txt"
  let rename_result = FileSystem::rename_file(test_file1, renamed_file)
  assert_true(rename_result.success)
  
  // 等待事件处理
  Thread::sleep(100)
  
  // 检查移动事件
  let move_events = FileMonitor::get_events(file_monitor, FileEventType::Moved)
  assert_true(move_events.length() >= 1)
  
  let test1_moved = move_events.some(fn(event) {
    event.old_path == test_file1 && event.new_path == renamed_file
  })
  assert_true(test1_moved)
  
  // 测试文件删除
  let delete_result = FileSystem::delete_file(renamed_file)
  assert_true(delete_result.success)
  
  let subdir_delete_result = FileSystem::delete_file(test_file2)
  assert_true(subdir_delete_result.success)
  
  // 等待事件处理
  Thread::sleep(100)
  
  // 检查删除事件
  let delete_events = FileMonitor::get_events(file_monitor, FileEventType::Deleted)
  assert_true(delete_events.length() >= 2)
  
  let renamed_deleted = delete_events.some(fn(event) {
    event.file_path == renamed_file
  })
  let test2_deleted = delete_events.some(fn(event) {
    event.file_path == test_file2
  })
  assert_true(renamed_deleted)
  assert_true(test2_deleted)
  
  // 获取监控统计
  let monitor_stats = FileMonitor::get_stats(file_monitor)
  assert_true(monitor_stats.total_events >= 6)
  assert_true(monitor_stats.events_processed >= 6)
  
  // 停止监控
  let stop_result = FileMonitor::stop(file_monitor)
  assert_true(stop_result.success)
  
  // 清理测试目录
  FileSystem::delete_directory(monitor_dir, true)
}

// 测试5: 文件压缩和解压
test "文件压缩和解压测试" {
  // 创建测试数据文件
  let source_file = "/tmp/azimuth_source_data.txt"
  let compressed_file = "/tmp/azimuth_compressed.gz"
  let decompressed_file = "/tmp/azimuth_decompressed.txt"
  
  // 生成大型文本数据
  let source_data = []
  for i in 1..=5000 {
    let line = "trace-" + String::pad_left(i.to_string(), 6, "0") + "," +
               "span-" + String::pad_left(i.to_string(), 6, "0") + "," +
               "azimuth-telemetry," +
               "operation-" + (i % 10).to_string() + "," +
               (1640995200000 + i * 1000).to_string() + "," +
               if i % 20 == 0 { "error" } else { "ok" } + "\n"
    source_data.push(line)
  }
  
  // 写入源文件
  FileSystem::write_file_lines(source_file, source_data)
  
  // 获取源文件大小
  let original_size = FileSystem::get_file_size(source_file)
  assert_true(original_size > 500000) // 大于500KB
  
  // 创建压缩器
  let compressor = Compressor::new(CompressionType::Gzip)
  compressor.set_compression_level(6) // 中等压缩级别
  
  // 压缩文件
  let compress_start_time = Clock::now_unix_nanos(Clock::system())
  let compress_result = compressor.compress_file(source_file, compressed_file)
  let compress_end_time = Clock::now_unix_nanos(Clock::system())
  
  assert_true(compress_result.success)
  
  // 验证压缩性能
  let compress_duration = compress_end_time - compress_start_time
  assert_true(compress_duration < 1000000000) // 小于1秒
  
  // 获取压缩后文件大小
  let compressed_size = FileSystem::get_file_size(compressed_file)
  assert_true(compressed_size < original_size) // 压缩后应该更小
  
  // 计算压缩比
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  assert_true(compression_ratio < 0.5) // 至少50%压缩率
  
  // 创建解压器
  let decompressor = Decompressor::new(CompressionType::Gzip)
  
  // 解压文件
  let decompress_start_time = Clock::now_unix_nanos(Clock::system())
  let decompress_result = decompressor.decompress_file(compressed_file, decompressed_file)
  let decompress_end_time = Clock::now_unix_nanos(Clock::system())
  
  assert_true(decompress_result.success)
  
  // 验证解压性能
  let decompress_duration = decompress_end_time - decompress_start_time
  assert_true(decompress_duration < 1000000000) // 小于1秒
  
  // 验证解压后文件大小
  let decompressed_size = FileSystem::get_file_size(decompressed_file)
  assert_eq(decompressed_size, original_size)
  
  // 验证内容完整性
  let original_lines = FileSystem::read_file_lines(source_file)
  let decompressed_lines = FileSystem::read_file_lines(decompressed_file)
  
  assert_eq(original_lines.length(), decompressed_lines.length())
  for i in 0..=original_lines.length() - 1 {
    assert_eq(original_lines[i], decompressed_lines[i])
  }
  
  // 测试流式压缩
  let stream_compressed = "/tmp/azimuth_stream_compressed.gz"
  let stream_compressor = StreamCompressor::new(CompressionType::Gzip)
  
  let stream_compress_start = Clock::now_unix_nanos(Clock::system())
  
  // 打开流式压缩
  let stream_result = stream_compressor.open(stream_compressed)
  assert_true(stream_result.success)
  
  // 分块写入数据
  let chunk_size = 1000
  for i in 0..=source_data.length() / chunk_size {
    let start_index = i * chunk_size
    let end_index = if start_index + chunk_size > source_data.length() {
      source_data.length()
    } else {
      start_index + chunk_size
    }
    
    let chunk = source_data.slice(start_index, end_index)
    let chunk_string = chunk.reduce(fn(acc, line) { acc + line }, "")
    
    let write_result = stream_compressor.write(chunk_string)
    assert_true(write_result.success)
  }
  
  // 关闭流式压缩
  let close_result = stream_compressor.close()
  assert_true(close_result.success)
  
  let stream_compress_end = Clock::now_unix_nanos(Clock::system())
  let stream_compress_duration = stream_compress_end - stream_compress_start
  
  // 验证流式压缩性能
  assert_true(stream_compress_duration < 2000000000) // 小于2秒
  
  // 验证流式压缩结果
  let stream_compressed_size = FileSystem::get_file_size(stream_compressed)
  assert_true(stream_compressed_size > 0)
  
  // 清理临时文件
  FileSystem::delete_file(source_file)
  FileSystem::delete_file(compressed_file)
  FileSystem::delete_file(decompressed_file)
  FileSystem::delete_file(stream_compressed)
}

// 测试6: 文件权限和安全
test "文件权限和安全测试" {
  // 创建测试文件
  let secure_file = "/tmp/azimuth_secure_test.txt"
  FileSystem::write_file(secure_file, "sensitive telemetry data")
  
  // 创建文件安全管理器
  let security_manager = FileSecurityManager::new()
  
  // 测试文件权限设置
  let read_only_permissions = FilePermissions::new()
  FilePermissions::set_owner_read(read_only_permissions, true)
  FilePermissions::set_group_read(read_only_permissions, true)
  FilePermissions::set_other_read(read_only_permissions, true)
  FilePermissions::set_owner_write(read_only_permissions, false)
  FilePermissions::set_group_write(read_only_permissions, false)
  FilePermissions::set_other_write(read_only_permissions, false)
  FilePermissions::set_owner_execute(read_only_permissions, false)
  FilePermissions::set_group_execute(read_only_permissions, false)
  FilePermissions::set_other_execute(read_only_permissions, false)
  
  // 应用只读权限
  let chmod_result = security_manager.chmod(secure_file, read_only_permissions)
  assert_true(chmod_result.success)
  
  // 验证权限
  let current_permissions = security_manager.get_permissions(secure_file)
  assert_true(current_permissions.owner_read)
  assert_false(current_permissions.owner_write)
  assert_false(current_permissions.owner_execute)
  
  // 测试只读文件写入限制
  let write_attempt = FileSystem::append_file(secure_file, "additional data")
  assert_false(write_attempt.success)
  
  // 测试文件所有者变更
  let owner_change_result = security_manager.chown(secure_file, "testuser", "testgroup")
  // 注意：这个测试可能需要root权限，在实际环境中可能跳过
  // assert_true(owner_change_result.success)
  
  // 创建加密文件写入器
  let encrypted_file = "/tmp/azimuth_encrypted.dat"
  let encryption_key = "azimuth_encryption_key_12345"
  let encryption_iv = "initialization_vector"
  
  let encrypted_writer = EncryptedFileWriter::new()
  encrypted_writer.set_algorithm(EncryptionAlgorithm::AES256)
  encrypted_writer.set_key(encryption_key)
  encrypted_writer.set_iv(encryption_iv)
  
  // 写入加密数据
  let sensitive_data = "user_id=12345,email=user@example.com,ssn=123-45-6789"
  let encrypt_result = encrypted_writer.write_file(encrypted_file, sensitive_data)
  assert_true(encrypt_result.success)
  
  // 验证加密文件内容不可读
  let encrypted_content = FileSystem::read_file(encrypted_file)
  assert_false(encrypted_content.contains("user@example.com"))
  assert_false(encrypted_content.contains("123-45-6789"))
  
  // 创建解密文件读取器
  let encrypted_reader = EncryptedFileReader::new()
  encrypted_reader.set_algorithm(EncryptionAlgorithm::AES256)
  encrypted_reader.set_key(encryption_key)
  encrypted_reader.set_iv(encryption_iv)
  
  // 读取解密数据
  let decrypt_result = encrypted_reader.read_file(encrypted_file)
  assert_true(decrypt_result.success)
  assert_eq(decrypt_result.content, sensitive_data)
  
  // 测试错误密钥解密
  let wrong_reader = EncryptedFileReader::new()
  wrong_reader.set_algorithm(EncryptionAlgorithm::AES256)
  wrong_reader.set_key("wrong_encryption_key")
  wrong_reader.set_iv(encryption_iv)
  
  let wrong_decrypt_result = wrong_reader.read_file(encrypted_file)
  assert_false(wrong_decrypt_result.success)
  
  // 测试文件完整性校验
  let integrity_file = "/tmp/azimuth_integrity_test.txt"
  let file_content = "telemetry data for integrity check"
  FileSystem::write_file(integrity_file, file_content)
  
  // 计算文件哈希
  let original_hash = security_manager.calculate_hash(integrity_file, HashAlgorithm::SHA256)
  assert_true(original_hash.length() > 0)
  
  // 验证文件完整性
  let integrity_check = security_manager.verify_integrity(integrity_file, original_hash, HashAlgorithm::SHA256)
  assert_true(integrity_check)
  
  // 修改文件内容
  FileSystem::append_file(integrity_file, " - modified")
  
  // 验证修改后的完整性检查失败
  let modified_integrity_check = security_manager.verify_integrity(integrity_file, original_hash, HashAlgorithm::SHA256)
  assert_false(modified_integrity_check)
  
  // 测试安全删除
  let secure_delete_file = "/tmp/azimuth_secure_delete.txt"
  FileSystem::write_file(secure_delete_file, "sensitive data to be securely deleted")
  
  // 安全删除文件（多次覆写）
  let secure_delete_result = security_manager.secure_delete(secure_delete_file, 3) // 覆写3次
  assert_true(secure_delete_result.success)
  
  // 验证文件已被删除
  assert_false(FileSystem::file_exists(secure_delete_file))
  
  // 清理临时文件
  security_manager.secure_delete(secure_file, 1)
  security_manager.secure_delete(encrypted_file, 1)
  security_manager.secure_delete(integrity_file, 1)
}

// 测试7: 文件系统操作和目录管理
test "文件系统操作和目录管理测试" {
  // 创建测试目录结构
  let base_dir = "/tmp/azimuth_fs_test"
  let subdirs = [
    base_dir + "/telemetry/data",
    base_dir + "/telemetry/logs",
    base_dir + "/telemetry/config",
    base_dir + "/backup/daily",
    base_dir + "/backup/weekly"
  ]
  
  // 创建目录结构
  for dir in subdirs {
    let create_result = FileSystem::create_directory(dir)
    assert_true(create_result.success)
    assert_true(FileSystem::directory_exists(dir))
  }
  
  // 在不同目录中创建文件
  let files = [
    (base_dir + "/telemetry/data/trace_data.json", '{"trace_id": "trace-001", "spans": []}'),
    (base_dir + "/telemetry/data/metrics.csv", 'timestamp,metric_name,value\n1640995200,cpu_usage,75.5'),
    (base_dir + "/telemetry/logs/app.log", '2023-01-01 00:00:00 INFO Application started'),
    (base_dir + "/telemetry/config/telemetry.yaml", 'sampling_rate: 0.1\nexport_interval: 60s'),
    (base_dir + "/backup/daily/backup_20230101.tar.gz", 'binary_backup_data'),
    (base_dir + "/backup/weekly/backup_week1.tar.gz", 'weekly_backup_data')
  ]
  
  for (file_path, content) in files {
    let write_result = FileSystem::write_file(file_path, content)
    assert_true(write_result.success)
    assert_true(FileSystem::file_exists(file_path))
  }
  
  // 测试目录遍历
  let dir_iterator = DirectoryIterator::new(base_dir)
  dir_iterator.set_recursive(true)
  dir_iterator.set_include_directories(true)
  dir_iterator.set_include_files(true)
  
  let mut found_files = []
  let mut found_dirs = []
  
  while dir_iterator.has_next() {
    let item = dir_iterator.next()
    match item.type {
      DirectoryItemType::File => found_files.push(item.path),
      DirectoryItemType::Directory => found_dirs.push(item.path)
    }
  }
  
  // 验证遍历结果
  assert_eq(found_files.length(), 6)
  assert_eq(found_dirs.length(), 5) // 包括base_dir
  
  // 测试文件搜索
  let search_pattern = "*.json"
  let search_results = FileSystem::search_files(base_dir, search_pattern, true)
  assert_eq(search_results.length(), 1)
  assert_true(search_results[0].ends_with("trace_data.json"))
  
  // 测试按扩展名过滤
  let yaml_files = FileSystem::filter_files_by_extension(found_files, "yaml")
  assert_eq(yaml_files.length(), 1)
  
  let log_files = FileSystem::filter_files_by_extension(found_files, "log")
  assert_eq(log_files.length(), 1)
  
  // 测试按大小过滤
  let large_files = FileSystem::filter_files_by_size(found_files, 10, 1000) // 10-1000字节
  assert_true(large_files.length() > 0)
  
  // 测试目录大小计算
  let dir_size = FileSystem::calculate_directory_size(base_dir)
  assert_true(dir_size > 0)
  
  // 测试磁盘空间信息
  let disk_info = FileSystem::get_disk_space_info("/tmp")
  assert_true(disk_info.total_space > 0)
  assert_true(disk_info.free_space > 0)
  assert_true(disk_info.used_space > 0)
  assert_true(disk_info.free_space < disk_info.total_space)
  
  // 测试文件复制
  let source_file = base_dir + "/telemetry/data/trace_data.json"
  let dest_file = base_dir + "/telemetry/data/trace_data_copy.json"
  
  let copy_result = FileSystem::copy_file(source_file, dest_file)
  assert_true(copy_result.success)
  assert_true(FileSystem::file_exists(dest_file))
  
  // 验证复制内容
  let source_content = FileSystem::read_file(source_file)
  let dest_content = FileSystem::read_file(dest_file)
  assert_eq(source_content, dest_content)
  
  // 测试文件移动
  let old_path = base_dir + "/telemetry/config/telemetry.yaml"
  let new_path = base_dir + "/telemetry/config/telemetry_renamed.yaml"
  
  let move_result = FileSystem::move_file(old_path, new_path)
  assert_true(move_result.success)
  assert_false(FileSystem::file_exists(old_path))
  assert_true(FileSystem::file_exists(new_path))
  
  // 测试批量操作
  let files_to_delete = [
    base_dir + "/telemetry/data/trace_data_copy.json",
    base_dir + "/backup/daily/backup_20230101.tar.gz"
  ]
  
  let batch_delete_result = FileSystem::delete_files(files_to_delete)
  assert_true(batch_delete_result.success)
  
  for file in files_to_delete {
    assert_false(FileSystem::file_exists(file))
  }
  
  // 测试目录树复制
  let source_dir = base_dir + "/telemetry"
  let dest_dir = base_dir + "/telemetry_copy"
  
  let tree_copy_result = FileSystem::copy_directory_tree(source_dir, dest_dir)
  assert_true(tree_copy_result.success)
  assert_true(FileSystem::directory_exists(dest_dir))
  
  // 验证目录树复制
  let source_files = FileSystem::list_files_recursive(source_dir)
  let dest_files = FileSystem::list_files_recursive(dest_dir)
  assert_eq(source_files.length(), dest_files.length())
  
  // 测试目录权限批量设置
  let dir_permissions = FilePermissions::new()
  FilePermissions::set_owner_all(dir_permissions, true)
  FilePermissions::set_group_read(dir_permissions, true)
  FilePermissions::set_other_read(dir_permissions, true)
  
  let security_manager = FileSecurityManager::new()
  let batch_chmod_result = security_manager.chmod_recursive(dest_dir, dir_permissions)
  assert_true(batch_chmod_result.success)
  
  // 清理测试目录
  FileSystem::delete_directory(base_dir, true)
  assert_false(FileSystem::directory_exists(base_dir))
}

// 测试8: 文件缓存和性能优化
test "文件缓存和性能优化测试" {
  // 创建文件缓存管理器
  let file_cache = FileCache::new()
  file_cache.set_max_size(100 * 1024 * 1024) // 100MB最大缓存
  file_cache.set_max_entries(1000) // 最多1000个缓存条目
  file_cache.set_ttl(300000) // 5分钟TTL
  
  // 创建测试文件
  let cache_test_files = []
  for i in 1..=10 {
    let file_path = "/tmp/azimuth_cache_test_" + i.to_string() + ".txt"
    let content = "cache test content " + i.to_string() + " - " + "x".repeat(1000 * i) // 不同大小的内容
    FileSystem::write_file(file_path, content)
    cache_test_files.push(file_path)
  }
  
  // 测试文件缓存读取
  let cache_read_start = Clock::now_unix_nanos(Clock::system())
  
  // 第一次读取（缓存未命中）
  let first_read_result = file_cache.read_file(cache_test_files[0])
  assert_true(first_read_result.success)
  assert_false(first_read_result.from_cache)
  
  let first_read_end = Clock::now_unix_nanos(Clock::system())
  let first_read_duration = first_read_end - cache_read_start
  
  // 第二次读取（缓存命中）
  let cache_hit_start = Clock::now_unix_nanos(Clock::system())
  
  let second_read_result = file_cache.read_file(cache_test_files[0])
  assert_true(second_read_result.success)
  assert_true(second_read_result.from_cache)
  assert_eq(second_read_result.content, first_read_result.content)
  
  let cache_hit_end = Clock::now_unix_nanos(Clock::system())
  let cache_hit_duration = cache_hit_end - cache_hit_start
  
  // 验证缓存性能提升
  assert_true(cache_hit_duration < first_read_duration)
  
  // 预热缓存
  for file in cache_test_files {
    file_cache.read_file(file)
  }
  
  // 验证缓存统计
  let cache_stats = file_cache.get_stats()
  assert_true(cache_stats.cache_hits > 0)
  assert_true(cache_stats.cache_misses > 0)
  assert_true(cache_stats.cache_size > 0)
  assert_true(cache_stats.entry_count > 0)
  
  // 测试缓存LRU淘汰
  let large_files = []
  for i in 1..=20 {
    let file_path = "/tmp/azimuth_large_cache_" + i.to_string() + ".txt"
    let content = "x".repeat(5 * 1024 * 1024) // 5MB文件
    FileSystem::write_file(file_path, content)
    large_files.push(file_path)
    
    // 读取文件以填充缓存
    file_cache.read_file(file_path)
  }
  
  // 验证缓存大小限制
  let updated_cache_stats = file_cache.get_stats()
  assert_true(updated_cache_stats.cache_size <= 100 * 1024 * 1024) // 不超过100MB
  assert_true(updated_cache_stats.entry_count <= 1000) // 不超过1000个条目
  
  // 测试缓存失效
  let invalidate_result = file_cache.invalidate(cache_test_files[0])
  assert_true(invalidate_result.success)
  
  // 再次读取应该从磁盘加载
  let invalidated_read_result = file_cache.read_file(cache_test_files[0])
  assert_true(invalidated_read_result.success)
  assert_false(invalidated_read_result.from_cache)
  
  // 测试批量缓存预热
  let preload_files = cache_test_files.slice(0, 5)
  let preload_result = file_cache.preload_files(preload_files)
  assert_true(preload_result.success)
  assert_eq(preload_result.loaded_files, 5)
  
  // 测试缓存清理
  let clear_result = file_cache.clear()
  assert_true(clear_result.success)
  
  let cleared_stats = file_cache.get_stats()
  assert_eq(cleared_stats.cache_size, 0)
  assert_eq(cleared_stats.entry_count, 0)
  
  // 测试异步文件读取
  let async_reader = AsyncFileReader::new()
  async_reader.set_thread_pool_size(4) // 4个工作线程
  async_reader.set_buffer_size(64 * 1024) // 64KB缓冲区
  
  // 启动异步读取任务
  let async_tasks = []
  for file in cache_test_files {
    let task = async_reader.read_file_async(file)
    async_tasks.push(task)
  }
  
  // 等待所有任务完成
  let async_results = []
  for task in async_tasks {
    let result = async_reader.wait_for_result(task)
    async_results.push(result)
  }
  
  // 验证异步读取结果
  assert_eq(async_results.length(), cache_test_files.length())
  for result in async_results {
    assert_true(result.success)
    assert_true(result.content.length() > 0)
  }
  
  // 验证异步读取性能
  let async_stats = async_reader.get_stats()
  assert_true(async_stats.total_reads >= 10)
  assert_true(async_stats.total_bytes > 0)
  assert_true(async_stats.average_read_time > 0)
  
  // 测试文件映射内存访问
  let mmap_file = cache_test_files[0]
  let mmap_reader = MemoryMappedFile::new(mmap_file)
  
  // 打开内存映射
  let mmap_open_result = mmap_reader.open()
  assert_true(mmap_open_result.success)
  
  // 读取内存映射数据
  let mmap_data = mmap_reader.read_all()
  assert_true(mmap_data.length() > 0)
  
  // 验证内存映射数据与原始数据一致
  let original_data = FileSystem::read_file(mmap_file)
  assert_eq(mmap_data, original_data)
  
  // 测试随机访问
  let file_size = FileSystem::get_file_size(mmap_file)
  if file_size > 100 {
    let offset = 50
    let length = 20
    let random_data = mmap_reader.read(offset, length)
    assert_eq(random_data.length(), length)
    
    let original_random = original_data.substring(offset, length)
    assert_eq(random_data, original_random)
  }
  
  // 关闭内存映射
  let mmap_close_result = mmap_reader.close()
  assert_true(mmap_close_result.success)
  
  // 清理测试文件
  let all_test_files = cache_test_files + large_files
  for file in all_test_files {
    FileSystem::delete_file(file)
  }
}