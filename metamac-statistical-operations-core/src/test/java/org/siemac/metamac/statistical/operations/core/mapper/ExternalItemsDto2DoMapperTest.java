package org.siemac.metamac.statistical.operations.core.mapper;

import static org.mockito.Mockito.times;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mockito;
import org.siemac.metamac.common.test.utils.MetamacMocks;
import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.ent.domain.ExternalItem;
import org.siemac.metamac.core.common.ent.domain.ExternalItemRepository;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.statistical.operations.core.utils.asserts.StatisticalOperationsAsserts;
import org.siemac.metamac.statistical.operations.core.utils.mocks.StatisticalOperationsMocks;

public class ExternalItemsDto2DoMapperTest {

    @Rule
    public ExpectedException    thrown        = ExpectedException.none();

    private static final String URN_01        = "lorem:ipsum:externalItem:mock:01";
    private static final String URN_02        = "lorem:ipsum:externalItem:mock:02";
    private static final String METADATA_NAME = "LOREM_IPSUM";

    private Dto2DoMapper        dto2doMapper  = new Dto2DoMapperImpl();

    @Test
    public void testExternalItemDtoToEntity() throws Exception {
        // NULL, NULL
        {
            testExternalItemDtoToEntity(null, null);
        }

        // EXISTS, NULL
        {
            ExternalItemDto externalItemDto = StatisticalOperationsMocks.mockExternalItemDtoComplete(URN_01, TypeExternalArtefactsEnum.AGENCY);
            testExternalItemDtoToEntity(externalItemDto, null);
        }

        // NULL, EXISTS
        {
            ExternalItemRepository repository = Mockito.mock(ExternalItemRepository.class);
            setRepositoryToMapper(repository);
            ExternalItem externalItem = StatisticalOperationsMocks.mockAgencyExternalItem();
            testExternalItemDtoToEntity(null, externalItem);
            Mockito.verify(repository).delete(Mockito.any(ExternalItem.class));
            Mockito.validateMockitoUsage();
        }

        // EXISTS, EXISTS
        {
            ExternalItemDto externalItemDto = MetamacMocks.mockExternalItemDtoComplete(URN_01, TypeExternalArtefactsEnum.AGENCY);
            ExternalItem externalItem = StatisticalOperationsMocks.mockAgencyExternalItem();
            testExternalItemDtoToEntity(externalItemDto, externalItem);
        }
    }

    @Test
    public void testExternalItemListToEntity() throws Exception {
        // EMPTY, EMPTY
        {
            Set<ExternalItemDto> dtos = new HashSet<ExternalItemDto>();
            Set<ExternalItem> entities = new HashSet<ExternalItem>();
            testExternalItemListToEntity(dtos, entities);
        }

        // EXISTS, EMPTY
        {
            Set<ExternalItemDto> dtos = new HashSet<ExternalItemDto>();
            dtos.add(StatisticalOperationsMocks.mockExternalItemDtoComplete(URN_01, TypeExternalArtefactsEnum.AGENCY));
            dtos.add(StatisticalOperationsMocks.mockExternalItemDtoComplete(URN_02, TypeExternalArtefactsEnum.AGENCY));
            Set<ExternalItem> entities = new HashSet<ExternalItem>();

            testExternalItemListToEntity(dtos, entities);
        }

        // EMPTY, EXISTS
        {
            Set<ExternalItemDto> dtos = new HashSet<ExternalItemDto>();
            Set<ExternalItem> entities = new HashSet<ExternalItem>();
            entities.add(StatisticalOperationsMocks.mockAgencyExternalItem());
            entities.add(StatisticalOperationsMocks.mockAgencyExternalItem());

            ExternalItemRepository repository = Mockito.mock(ExternalItemRepository.class);
            setRepositoryToMapper(repository);
            
            testExternalItemListToEntity(dtos, entities);
            
            Mockito.verify(repository, times(2)).delete(Mockito.any(ExternalItem.class));
            Mockito.validateMockitoUsage();
        }

        // EXISTS, EXISTS: Same elements 
        {
            Set<ExternalItemDto> dtos = new HashSet<ExternalItemDto>();
            dtos.add(new ExternalItemDto("CODE_01", "URI_01", "URN_01", TypeExternalArtefactsEnum.AGENCY));
            dtos.add(new ExternalItemDto("CODE_02", "URI_02", "URN_02", TypeExternalArtefactsEnum.CATEGORY));
            Set<ExternalItem> entities = new HashSet<ExternalItem>();
            entities.add(new ExternalItem("CODE_01", "URI_01", "URN_01", TypeExternalArtefactsEnum.AGENCY));
            entities.add(new ExternalItem("CODE_02", "URI_02", "URN_02", TypeExternalArtefactsEnum.CATEGORY));

            testExternalItemListToEntity(dtos, entities);
        }

        // EXISTS, EXISTS: More elements
        {
            Set<ExternalItemDto> dtos = new HashSet<ExternalItemDto>();
            dtos.add(new ExternalItemDto("CODE_01", "URI_01", "URN_01", TypeExternalArtefactsEnum.AGENCY));
            dtos.add(new ExternalItemDto("CODE_02", "URI_02", "URN_02", TypeExternalArtefactsEnum.CATEGORY));
            dtos.add(new ExternalItemDto("CODE_03", "URI_03", "URN_03", TypeExternalArtefactsEnum.CATEGORY));
            Set<ExternalItem> entities = new HashSet<ExternalItem>();
            entities.add(new ExternalItem("CODE_01", "URI_01", "URN_01", TypeExternalArtefactsEnum.AGENCY));
            entities.add(new ExternalItem("CODE_02", "URI_02", "URN_02", TypeExternalArtefactsEnum.CATEGORY));

            testExternalItemListToEntity(dtos, entities);
        }

        // EXISTS, EXISTS: Less elements
        {
            Set<ExternalItemDto> dtos = new HashSet<ExternalItemDto>();
            dtos.add(new ExternalItemDto("CODE_01", "URI_01", "URN_01", TypeExternalArtefactsEnum.AGENCY));
            dtos.add(new ExternalItemDto("CODE_02", "URI_02", "URN_02", TypeExternalArtefactsEnum.CATEGORY));
            Set<ExternalItem> entities = new HashSet<ExternalItem>();
            entities.add(new ExternalItem("CODE_01", "URI_01", "URN_01", TypeExternalArtefactsEnum.AGENCY));
            entities.add(new ExternalItem("CODE_02", "URI_02", "URN_02", TypeExternalArtefactsEnum.CATEGORY));
            entities.add(new ExternalItem("CODE_03", "URI_03", "URN_03", TypeExternalArtefactsEnum.CATEGORY));

            ExternalItemRepository repository = Mockito.mock(ExternalItemRepository.class);
            setRepositoryToMapper(repository);
            
            testExternalItemListToEntity(dtos, entities);
            
            Mockito.verify(repository).delete(Mockito.any(ExternalItem.class));
            Mockito.validateMockitoUsage();
        }

        // EXISTS, EXISTS: Different elements
        {
            Set<ExternalItemDto> dtos = new HashSet<ExternalItemDto>();
            dtos.add(StatisticalOperationsMocks.mockExternalItemDtoComplete(URN_01, TypeExternalArtefactsEnum.AGENCY));
            dtos.add(StatisticalOperationsMocks.mockExternalItemDtoComplete(URN_02, TypeExternalArtefactsEnum.AGENCY));
            Set<ExternalItem> entities = new HashSet<ExternalItem>();
            entities.add(StatisticalOperationsMocks.mockAgencyExternalItem());
            entities.add(StatisticalOperationsMocks.mockAgencyExternalItem());

            ExternalItemRepository repository = Mockito.mock(ExternalItemRepository.class);
            setRepositoryToMapper(repository);
            
            testExternalItemListToEntity(dtos, entities);
            
            Mockito.verify(repository, times(2)).delete(Mockito.any(ExternalItem.class));
            Mockito.validateMockitoUsage();
        }
    }

    @Test
    public void testExternalItemListToEntityErrorExpectedAndActualNull() throws Exception {
        thrown.expect(InvocationTargetException.class);
        testExternalItemListToEntity(null, null);
    }

    @Test
    public void testExternalItemListToEntityErrorExpectedNull() throws Exception {
        thrown.expect(InvocationTargetException.class);
        Set<ExternalItem> entities = new HashSet<ExternalItem>();
        testExternalItemListToEntity(null, entities);
    }

    @Test
    public void testExternalItemListToEntityErrorActualNull() throws Exception {
        thrown.expect(InvocationTargetException.class);
        Set<ExternalItemDto> dtos = new HashSet<ExternalItemDto>();
        testExternalItemListToEntity(dtos, null);
    }

    private void setRepositoryToMapper(ExternalItemRepository repository) throws Exception {
        Field externalItemRepository = dto2doMapper.getClass().getDeclaredField("externalItemRepository");
        externalItemRepository.setAccessible(true);
        externalItemRepository.set(dto2doMapper, repository);
    }

    @SuppressWarnings("rawtypes")
    private Method getVisibleExternalItemDtoToEntityMethod() throws Exception {
        Class[] parameterTypes = new Class[3];
        parameterTypes[0] = ExternalItemDto.class;
        parameterTypes[1] = ExternalItem.class;
        parameterTypes[2] = String.class;
        Method externalItemDtoToEntity = dto2doMapper.getClass().getDeclaredMethod("externalItemDtoToEntity", parameterTypes);
        externalItemDtoToEntity.setAccessible(true);
        return externalItemDtoToEntity;
    }

    private void testExternalItemDtoToEntity(ExternalItemDto externalItemDto, ExternalItem externalItem) throws Exception {
        Method externalItemDtoToEntity = getVisibleExternalItemDtoToEntityMethod();

        Object[] parameters = new Object[3];
        parameters[0] = externalItemDto;
        parameters[1] = externalItem;
        parameters[2] = METADATA_NAME;

        ExternalItem result = (ExternalItem) externalItemDtoToEntity.invoke(dto2doMapper, parameters);
        StatisticalOperationsAsserts.assertEqualsExternalItem(result, externalItemDto);
    }

    @SuppressWarnings("rawtypes")
    private Method getVisibleExternalItemListToEntityMethod() throws Exception {
        Class[] parameterTypes = new Class[3];
        parameterTypes[0] = Set.class;
        parameterTypes[1] = Set.class;
        parameterTypes[2] = String.class;
        Method externalItemListToEntity = dto2doMapper.getClass().getDeclaredMethod("externalItemListToEntity", parameterTypes);
        externalItemListToEntity.setAccessible(true);
        return externalItemListToEntity;
    }

    @SuppressWarnings("unchecked")
    private void testExternalItemListToEntity(Set<ExternalItemDto> dtos, Set<ExternalItem> entities) throws Exception {
        Method externalItemListToEntity = getVisibleExternalItemListToEntityMethod();

        Object[] parameters = new Object[3];
        parameters[0] = dtos;
        parameters[1] = entities;
        parameters[2] = METADATA_NAME;

        Set<ExternalItem> result = (Set<ExternalItem>) externalItemListToEntity.invoke(dto2doMapper, parameters);
        StatisticalOperationsAsserts.assertEqualsExternalItemCollectionMapper(result, dtos);

    }
}
