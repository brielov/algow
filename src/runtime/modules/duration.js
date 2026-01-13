// Duration module - cross-platform
// Duration is stored as milliseconds internally
$foreign.Duration = {
  zero: 0,

  millis: (n) => n,
  seconds: (n) => n * 1000,
  minutes: (n) => n * 60 * 1000,
  hours: (n) => n * 60 * 60 * 1000,
  days: (n) => n * 24 * 60 * 60 * 1000,
  weeks: (n) => n * 7 * 24 * 60 * 60 * 1000,

  add: (a) => (b) => a + b,
  negate: (d) => -d,

  toMillis: (d) => d,

  toIso: (d) => {
    const abs = Math.abs(d);
    const sign = d < 0 ? "-" : "";
    const hours = Math.floor(abs / 3600000);
    const minutes = Math.floor((abs % 3600000) / 60000);
    const seconds = Math.floor((abs % 60000) / 1000);
    const millis = abs % 1000;

    let result = sign + "PT";
    if (hours > 0) result += hours + "H";
    if (minutes > 0) result += minutes + "M";
    if (seconds > 0 || millis > 0 || result === sign + "PT") {
      if (millis > 0) {
        result += seconds + "." + String(millis).padStart(3, "0") + "S";
      } else {
        result += seconds + "S";
      }
    }
    return result;
  },

  fromIso: (s) => {
    const match = s.match(/^(-)?PT(?:(\d+)H)?(?:(\d+)M)?(?:(\d+(?:\.\d+)?)S)?$/);
    if (!match) return [0];
    const sign = match[1] ? -1 : 1;
    const hours = parseInt(match[2] || "0", 10);
    const minutes = parseInt(match[3] || "0", 10);
    const seconds = parseFloat(match[4] || "0");
    const ms = sign * (hours * 3600000 + minutes * 60000 + seconds * 1000);
    return [1, ms];
  },
};
