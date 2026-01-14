// DateTime module - cross-platform
// DateTime is stored as { date, time } where date and time are from their respective modules

const $DateTime_make = (date) => (time) => ({ date, time });

const $DateTime_date = (dt) => dt.date;

const $DateTime_time = (dt) => dt.time;

const $DateTime_now = (zone) => {
  const d = new Date();
  const str = d.toLocaleString("en-US", {
    timeZone: zone,
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
    hour12: false,
  });
  // Parse "MM/DD/YYYY, HH:MM:SS"
  const match = str.match(/(\d{2})\/(\d{2})\/(\d{4}), (\d{2}):(\d{2}):(\d{2})/);
  if (!match) {
    // Fallback to UTC
    return {
      date: { y: d.getUTCFullYear(), m: d.getUTCMonth() + 1, d: d.getUTCDate() },
      time: { h: d.getUTCHours(), m: d.getUTCMinutes(), s: d.getUTCSeconds() },
    };
  }
  return {
    date: {
      y: parseInt(match[3], 10),
      m: parseInt(match[1], 10),
      d: parseInt(match[2], 10),
    },
    time: {
      h: parseInt(match[4], 10) % 24,
      m: parseInt(match[5], 10),
      s: parseInt(match[6], 10),
    },
  };
};

const $DateTime_toTimestamp = (zone) => (dt) => {
  // Create a date string and parse it in the given timezone
  const { date, time } = dt;
  const isoStr = `${String(date.y).padStart(4, "0")}-${String(date.m).padStart(2, "0")}-${String(date.d).padStart(2, "0")}T${String(time.h).padStart(2, "0")}:${String(time.m).padStart(2, "0")}:${String(time.s).padStart(2, "0")}`;

  if (zone === "UTC") {
    return Date.parse(isoStr + "Z");
  }

  // For other timezones, we need to find the offset
  // Create a date and get its offset in the target timezone
  const d = new Date(isoStr + "Z");
  const utcStr = d.toLocaleString("en-US", { timeZone: "UTC" });
  const zoneStr = d.toLocaleString("en-US", { timeZone: zone });

  // Parse both to find the offset
  const parseLocale = (s) => {
    const m = s.match(/(\d+)\/(\d+)\/(\d+),?\s*(\d+):(\d+):(\d+)/);
    if (!m) return 0;
    return new Date(m[3], m[1] - 1, m[2], m[4], m[5], m[6]).getTime();
  };

  const offset = parseLocale(utcStr) - parseLocale(zoneStr);
  return Date.parse(isoStr + "Z") + offset;
};

const $DateTime_fromTimestamp = (zone) => (ts) => {
  const d = new Date(ts);
  const str = d.toLocaleString("en-US", {
    timeZone: zone,
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
    hour12: false,
  });
  const match = str.match(/(\d{2})\/(\d{2})\/(\d{4}), (\d{2}):(\d{2}):(\d{2})/);
  if (!match) {
    return {
      date: { y: d.getUTCFullYear(), m: d.getUTCMonth() + 1, d: d.getUTCDate() },
      time: { h: d.getUTCHours(), m: d.getUTCMinutes(), s: d.getUTCSeconds() },
    };
  }
  return {
    date: {
      y: parseInt(match[3], 10),
      m: parseInt(match[1], 10),
      d: parseInt(match[2], 10),
    },
    time: {
      h: parseInt(match[4], 10) % 24,
      m: parseInt(match[5], 10),
      s: parseInt(match[6], 10),
    },
  };
};

const $DateTime_add = (duration) => (dt) => {
  const ts = $DateTime_toTimestamp("UTC")(dt);
  return $DateTime_fromTimestamp("UTC")(ts + duration);
};

const $DateTime_sub = (duration) => (dt) => {
  const ts = $DateTime_toTimestamp("UTC")(dt);
  return $DateTime_fromTimestamp("UTC")(ts - duration);
};

const $DateTime_diff = (a) => (b) => {
  const tsA = $DateTime_toTimestamp("UTC")(a);
  const tsB = $DateTime_toTimestamp("UTC")(b);
  return tsB - tsA;
};

const $DateTime_toIso = (dt) => {
  const { date, time } = dt;
  const y = String(date.y).padStart(4, "0");
  const mo = String(date.m).padStart(2, "0");
  const d = String(date.d).padStart(2, "0");
  const h = String(time.h).padStart(2, "0");
  const mi = String(time.m).padStart(2, "0");
  const s = String(time.s).padStart(2, "0");
  return `${y}-${mo}-${d}T${h}:${mi}:${s}`;
};

const $DateTime_fromIso = (str) => {
  const match = str.match(/^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})$/);
  if (!match) return [0];
  const year = parseInt(match[1], 10);
  const month = parseInt(match[2], 10);
  const day = parseInt(match[3], 10);
  const hour = parseInt(match[4], 10);
  const minute = parseInt(match[5], 10);
  const second = parseInt(match[6], 10);

  const dateResult = $Date_make(year)(month)(day);
  if (dateResult[0] === 0) return [0];

  const timeResult = $Time_make(hour)(minute)(second);
  if (timeResult[0] === 0) return [0];

  return [1, { date: dateResult[1], time: timeResult[1] }];
};

const $DateTime_format = (zone) => (locale) => (dt) => {
  const ts = $DateTime_toTimestamp(zone)(dt);
  const d = new Date(ts);
  return new Intl.DateTimeFormat(locale, {
    timeZone: zone,
    dateStyle: "long",
    timeStyle: "medium",
  }).format(d);
};
