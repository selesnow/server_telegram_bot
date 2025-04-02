-- bot_users definition

CREATE TABLE bot_users (
    user_id  BIGINT PRIMARY KEY
                    UNIQUE,
    is_bot        BOOLEAN,
    first_name    VARCHAR(100),
    last_name     VARCHAR(100),
    last_activity DATETIME
, username TEXT);

-- chat_data definition

CREATE TABLE chat_data (
    chat_id  BIGINT,
    username TEXT,
    login    TEXT,
    CONSTRAINT new_pk PRIMARY KEY (chat_id, username)
);

-- chat_state definition

CREATE TABLE chat_state (
    chat_id  BIGINT,
    username TEXT,
    state    TEXT,
    CONSTRAINT new_pk PRIMARY KEY (chat_id, username)
);

-- forget_queue definition

CREATE TABLE forget_queue (id INTEGER PRIMARY KEY AUTOINCREMENT, task_name TEXT, quiet_till DATETIME NOT NULL);

-- log_entities definition

CREATE TABLE log_entities (
	message_id BIGINT,
	"offset" INTEGER,
	"length" INTEGER,
	"type" VARCHAR(50),
	url TEXT
);

-- log_errors definition

CREATE TABLE log_errors (
	session_id INTEGER NOT NULL,
	message_id BIGINT NOT NULL,
	error_text TEXT
, "timestamp" DATETIME NOT NULL, stop_reason VARCHAR(20) NOT NULL, "level" INTEGER, session_duratin_min NUMERIC, last_update_file TEXT, session_start_at DATETIME);

-- log_session_start definition

CREATE TABLE log_session_start (
	session_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
	"timestamp" DATETIME NOT NULL
);

-- log_work definition

CREATE TABLE log_work(
  session_id INT,
  message_id INT,
  bot_function TEXT,
  chat_id INT,
  chat_title TEXT,
  user_id INT,
  msg_text TEXT,
  cb_data TEXT,
  timestamp NUM,
  level INT
);

-- restart_task_log definition

CREATE TABLE `restart_task_log` (
  `task_name` TEXT,
  `last_result` TEXT,
  `last_run_time` DATETIME,
  `attemp` REAL
, rerun_time DATETIME);

-- task_info_daily definition

CREATE TABLE `task_info_daily` (
  `host_name` TEXT,
  `task_name` TEXT,
  `next_run_time` TEXT,
  `status` TEXT,
  `logon_mode` TEXT,
  `last_run_time` REAL,
  `last_result` TEXT,
  `author` TEXT,
  `task_to_run` TEXT,
  `start_in` TEXT,
  `comment` TEXT,
  `scheduled_task_state` TEXT,
  `idle_time` TEXT,
  `power_management` TEXT,
  `run_as_user` TEXT,
  `delete_task_if_not_rescheduled` TEXT,
  `stop_task_if_runs_x_hours_and_x_mins` TEXT,
  `schedule` TEXT,
  `schedule_type` TEXT,
  `start_time` TEXT,
  `start_date` TEXT,
  `end_date` TEXT,
  `days` TEXT,
  `months` TEXT,
  `repeat_every` TEXT,
  `repeat_until_time` TEXT,
  `repeat_until_duration` TEXT,
  `repeat_stop_if_still_running` TEXT,
  `log_time` TEXT
);

-- task_log definition

CREATE TABLE task_log (host_name TEXT, task_name TEXT, next_run_time TEXT, status TEXT, logon_mode TEXT, last_run_time REAL, last_result TEXT, author TEXT, task_to_run TEXT, start_in TEXT, comment TEXT, scheduled_task_state TEXT, idle_time TEXT, power_management TEXT, run_as_user TEXT, delete_task_if_not_rescheduled TEXT, stop_task_if_runs_x_hours_and_x_mins TEXT, schedule TEXT, schedule_type TEXT, start_time TEXT, start_date TEXT, end_date TEXT, days TEXT, months TEXT, repeat_every TEXT, repeat_until_time TEXT, repeat_until_duration TEXT, repeat_stop_if_still_running TEXT, log_time DATETIME);

-- users_previlegies definition

CREATE TABLE users_previlegies (
    user_id      BIGINT,
    command      VARCHAR(50),
    add_datetime DATETIME,
    CONSTRAINT new_pk PRIMARY KEY (user_id, command)
);