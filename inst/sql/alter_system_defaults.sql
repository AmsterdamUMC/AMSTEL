-- Defaults in PostgreSQL 15
ALTER SYSTEM SET wal_level = replica;
ALTER SYSTEM SET archive_mode = off;
ALTER SYSTEM SET max_wal_senders = 10;
ALTER SYSTEM SET max_wal_size = '1GB';
ALTER SYSTEM SET maintenance_work_mem = '64MB';
ALTER SYSTEM SET shared_buffers = '128MB';
