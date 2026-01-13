// Time module - cross-platform
// Time is stored as { h, m, s } (hour 0-23, minute 0-59, second 0-59)
$foreign.Time = {
  make: (hour) => (minute) => (second) => {
    if (hour < 0 || hour > 23 || minute < 0 || minute > 59 || second < 0 || second > 59) {
      return [0];
    }
    return [1, { h: hour, m: minute, s: second }];
  },

  hour: (time) => time.h,
  minute: (time) => time.m,
  second: (time) => time.s,

  addSeconds: (n) => (time) => {
    let total = time.h * 3600 + time.m * 60 + time.s + n;
    // Normalize to 0-86399 (seconds in a day)
    total = ((total % 86400) + 86400) % 86400;
    const h = Math.floor(total / 3600);
    const m = Math.floor((total % 3600) / 60);
    const s = total % 60;
    return { h, m, s };
  },

  addMinutes: (n) => (time) => $foreign.Time.addSeconds(n * 60)(time),

  addHours: (n) => (time) => $foreign.Time.addSeconds(n * 3600)(time),

  toIso: (time) => {
    const h = String(time.h).padStart(2, "0");
    const m = String(time.m).padStart(2, "0");
    const s = String(time.s).padStart(2, "0");
    return `${h}:${m}:${s}`;
  },

  fromIso: (str) => {
    const match = str.match(/^(\d{2}):(\d{2}):(\d{2})$/);
    if (!match) return [0];
    const hour = parseInt(match[1], 10);
    const minute = parseInt(match[2], 10);
    const second = parseInt(match[3], 10);
    return $foreign.Time.make(hour)(minute)(second);
  },

  format: (locale) => (time) => {
    const d = new Date(2000, 0, 1, time.h, time.m, time.s);
    return new Intl.DateTimeFormat(locale, { timeStyle: "medium" }).format(d);
  },
};
