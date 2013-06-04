package org.siemac.metamac.statistical.operations.core.mapper;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;

import org.dozer.DozerBeanMapper;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.siemac.metamac.common.test.utils.MetamacAsserts.MapperEnum;
import org.siemac.metamac.core.common.conf.ConfigurationService;
import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.ent.domain.ExternalItem;
import org.siemac.metamac.statistical.operations.core.utils.asserts.StatisticalOperationsAsserts;
import org.siemac.metamac.statistical.operations.core.utils.mocks.StatisticalOperationsMocks;
import org.siemac.metamac.common.test.mock.ConfigurationServiceMockImpl;

public class ExternalItemsDo2DtoMapperTest {

    @Rule
    public ExpectedException     thrown               = ExpectedException.none();

    private Do2DtoMapper         do2DtoMapper         = new Do2DtoMapperImpl();

    private ConfigurationService configurationService = new ConfigurationServiceMockImpl();
    private DozerBeanMapper      dozerBeanMapper      = new DozerBeanMapper();

    @Before
    public void setConfigurationToMapper() throws Exception {
        setFieldToBaseMapper("configurationService", configurationService);
        setFieldToMapper("mapper", dozerBeanMapper);
    }

    @Test
    public void testExternalItemDoToDtoWithNullParameter() throws Exception {
        testExternalItemDoToDto(null);
    }

    @Test
    public void testExternalItemDoToDtoWithExistsParameter() throws Exception {
        testExternalItemDoToDto(StatisticalOperationsMocks.mockAgencyExternalItem());
    }

    @Test
    public void testExternalItemDoCollectionToDtoCollectionWithNullParameter() throws Exception {
        testExternalItemDoCollectionToDtoCollection(null);
    }

    @Test
    public void testExternalItemDoCollectionToDtoCollectionWithEmptyParameter() throws Exception {
        testExternalItemDoCollectionToDtoCollection(new HashSet<ExternalItem>());
    }

    @Test
    public void testExternalItemDoCollectionToDtoCollectionWithExistsParameter() throws Exception {
        Set<ExternalItem> entities = new HashSet<ExternalItem>();
        entities.add(StatisticalOperationsMocks.mockAgencyExternalItem());
        entities.add(StatisticalOperationsMocks.mockAgencyExternalItem());

        testExternalItemDoCollectionToDtoCollection(entities);
    }


    private void setFieldToBaseMapper(String fieldName, ConfigurationService fieldValue) throws Exception {
        Field field = do2DtoMapper.getClass().getSuperclass().getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(do2DtoMapper, fieldValue);
    }
    
    private void setFieldToMapper(String fieldName, Object fieldValue) throws Exception {
        Field field = do2DtoMapper.getClass().getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(do2DtoMapper, fieldValue);
    }

    @SuppressWarnings("rawtypes")
    private Method getVisibleExternalItemDoToDtoMethod() throws Exception {
        Class[] parameterTypes = new Class[1];
        parameterTypes[0] = ExternalItem.class;
        Method externalItemDoToDtoMethod = do2DtoMapper.getClass().getDeclaredMethod("externalItemToDto", parameterTypes);
        externalItemDoToDtoMethod.setAccessible(true);
        return externalItemDoToDtoMethod;
    }

    private void testExternalItemDoToDto(ExternalItem externalItem) throws Exception {
        Method externalItemDtoToEntityMethod = getVisibleExternalItemDoToDtoMethod();

        Object[] parameters = new Object[1];
        parameters[0] = externalItem;

        ExternalItemDto result = (ExternalItemDto) externalItemDtoToEntityMethod.invoke(do2DtoMapper, parameters);
        StatisticalOperationsAsserts.assertEqualsExternalItem(externalItem, result, MapperEnum.DO2DTO);
    }

    @SuppressWarnings("rawtypes")
    private Method getVisibleExternalItemListToDtoMethod() throws Exception {
        Class[] parameterTypes = new Class[1];
        parameterTypes[0] = Set.class;
        Method externalItemListToEntityMethod = do2DtoMapper.getClass().getDeclaredMethod("externalItemListToDto", parameterTypes);
        externalItemListToEntityMethod.setAccessible(true);
        return externalItemListToEntityMethod;
    }

    @SuppressWarnings("unchecked")
    private void testExternalItemDoCollectionToDtoCollection(Set<ExternalItem> entities) throws Exception {
        Method externalItemListToEntityMethod = getVisibleExternalItemListToDtoMethod();

        Object[] parameters = new Object[1];
        parameters[0] = entities;

        Set<ExternalItemDto> result = (Set<ExternalItemDto>) externalItemListToEntityMethod.invoke(do2DtoMapper, parameters);
        StatisticalOperationsAsserts.assertEqualsExternalItemCollectionMapper(entities, result, MapperEnum.DO2DTO);

    }
}
