package org.siemac.metamac.statistical.operations.core.stream.mappers;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @param <D> Domain entity
 * @param <A> Avro entity
 */
public interface Do2AvroMapper<D, A> {
    A toAvro(D source);

    default List<A> toAvros(Collection<D> source) {
        if (source == null) {
            return new ArrayList<>();
        }
        return source.stream().map(this::toAvro).collect(Collectors.toList());
    }
}
