use std::fmt;

#[derive(PartialEq, PartialOrd, Debug)]
pub struct Clock {
    hours: i32,
    minutes: i32,
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {

        let mut constructor_hours = hours;
        let mut constructor_minutes = minutes;

        while constructor_minutes < 0{
            constructor_minutes += 60;
            constructor_hours -= 1;
        }

        while constructor_minutes >= 60 {
            constructor_minutes -= 60;
            constructor_hours += 1;
        }

        while constructor_hours < 0 {
            constructor_hours += 24;
        }

        while constructor_hours >= 24 {
            constructor_hours -= 24;
        }

        Clock{hours: constructor_hours, minutes: constructor_minutes}

    }

    pub fn add_minutes(&self, minutes: i32) -> Self {

        Self::new( self.hours + minutes/60, self.minutes + minutes%60)

    }

    pub fn compare(&self, other: &Clock) -> bool {
        self.hours == other.hours && self.minutes == other.minutes
    }

}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:02}:{:02}", self.hours, self.minutes)
    }
}