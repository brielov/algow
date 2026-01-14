// Date module - cross-platform
// Date is stored as { y, m, d } (year, month 1-12, day 1-31)

const $Date_make = (year) => (month) => (day) => {
  if (month < 1 || month > 12 || day < 1) return [0];
  const d = new Date(year, month - 1, day);
  if (d.getFullYear() !== year || d.getMonth() !== month - 1 || d.getDate() !== day) {
    return [0];
  }
  return [1, { y: year, m: month, d: day }];
};

const $Date_year = (date) => date.y;

const $Date_month = (date) => date.m;

const $Date_day = (date) => date.d;

const $Date_dayOfWeek = (date) => {
  const d = new Date(date.y, date.m - 1, date.d);
  const dow = d.getDay();
  return dow === 0 ? 7 : dow; // Convert Sunday=0 to Sunday=7 (ISO)
};

const $Date_dayOfYear = (date) => {
  const start = new Date(date.y, 0, 1);
  const current = new Date(date.y, date.m - 1, date.d);
  return Math.floor((current - start) / 86400000) + 1;
};

const $Date_isLeapYear = (date) => {
  const y = date.y;
  return (y % 4 === 0 && y % 100 !== 0) || y % 400 === 0;
};

const $Date_addDays = (n) => (date) => {
  const d = new Date(date.y, date.m - 1, date.d + n);
  return { y: d.getFullYear(), m: d.getMonth() + 1, d: d.getDate() };
};

const $Date_addMonths = (n) => (date) => {
  const d = new Date(date.y, date.m - 1 + n, date.d);
  // Handle overflow (e.g., Jan 31 + 1 month = Feb 28/29)
  if (d.getDate() !== date.d) {
    d.setDate(0); // Go to last day of previous month
  }
  return { y: d.getFullYear(), m: d.getMonth() + 1, d: d.getDate() };
};

const $Date_addYears = (n) => (date) => {
  const d = new Date(date.y + n, date.m - 1, date.d);
  // Handle Feb 29 on non-leap years
  if (d.getDate() !== date.d) {
    d.setDate(0);
  }
  return { y: d.getFullYear(), m: d.getMonth() + 1, d: d.getDate() };
};

const $Date_diffDays = (a) => (b) => {
  const da = new Date(a.y, a.m - 1, a.d);
  const db = new Date(b.y, b.m - 1, b.d);
  return Math.round((db - da) / 86400000);
};

const $Date_startOfMonth = (date) => ({ y: date.y, m: date.m, d: 1 });

const $Date_endOfMonth = (date) => {
  const d = new Date(date.y, date.m, 0);
  return { y: d.getFullYear(), m: d.getMonth() + 1, d: d.getDate() };
};

const $Date_startOfYear = (date) => ({ y: date.y, m: 1, d: 1 });

const $Date_endOfYear = (date) => ({ y: date.y, m: 12, d: 31 });

const $Date_toIso = (date) => {
  const y = String(date.y).padStart(4, "0");
  const m = String(date.m).padStart(2, "0");
  const d = String(date.d).padStart(2, "0");
  return `${y}-${m}-${d}`;
};

const $Date_fromIso = (s) => {
  const match = s.match(/^(\d{4})-(\d{2})-(\d{2})$/);
  if (!match) return [0];
  const year = parseInt(match[1], 10);
  const month = parseInt(match[2], 10);
  const day = parseInt(match[3], 10);
  return $Date_make(year)(month)(day);
};

const $Date_format = (locale) => (date) => {
  const d = new Date(date.y, date.m - 1, date.d);
  return new Intl.DateTimeFormat(locale, { dateStyle: "long" }).format(d);
};

const $Date_formatMonth = (locale) => (date) => {
  const d = new Date(date.y, date.m - 1, date.d);
  return new Intl.DateTimeFormat(locale, { month: "long" }).format(d);
};

const $Date_formatDay = (locale) => (date) => {
  const d = new Date(date.y, date.m - 1, date.d);
  return new Intl.DateTimeFormat(locale, { weekday: "long" }).format(d);
};
