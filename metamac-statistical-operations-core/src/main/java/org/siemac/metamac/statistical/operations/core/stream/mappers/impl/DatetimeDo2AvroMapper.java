package org.siemac.metamac.statistical.operations.core.stream.mappers.impl;

import org.joda.time.DateTime;
import org.siemac.metamac.statistical.operations.core.stream.mappers.Do2AvroMapper;
import org.siemac.metamac.statistical.operations.core.stream.messages.DatetimeAvro;
import org.springframework.stereotype.Component;

@Component
public class DatetimeDo2AvroMapper implements Do2AvroMapper<DateTime, DatetimeAvro> {
    @Override
    public DatetimeAvro toAvro(DateTime source) {
        if (source == null) {
            return null;
        }
        return DatetimeAvro.newBuilder().setInstant(source.toInstant().getMillis()).setTimezone(source.getZone().getID()).build();
    }
}
