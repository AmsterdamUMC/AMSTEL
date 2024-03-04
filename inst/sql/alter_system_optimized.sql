ALTER SYSTEM SET wal_level = minimal;
ALTER SYSTEM SET archive_mode = off;
ALTER SYSTEM SET max_wal_senders = 0;
ALTER SYSTEM SET max_wal_size = '128GB';
ALTER SYSTEM SET maintenance_work_mem = '1GB';
ALTER SYSTEM SET shared_buffers = '4GB';
