package org.siemac.metamac.statistical.operations.core.mapper;

import static org.mockito.Mockito.times;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mockito;
import org.siemac.metamac.common.test.utils.MetamacAsserts.MapperEnum;
import org.siemac.metamac.common.test.utils.MetamacMocks;
import org.siemac.metamac.core.common.conf.ConfigurationService;
import org.siemac.metamac.core.common.constants.CoreCommonConstants;
import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.ent.domain.ExternalItem;
import org.siemac.metamac.core.common.ent.domain.ExternalItemRepository;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.statistical.operations.core.utils.asserts.StatisticalOperationsAsserts;
import org.siemac.metamac.statistical.operations.core.utils.mocks.StatisticalOperationsMocks;
import org.siemac.metamac.common.test.constants.ConfigurationMockConstants;
import org.siemac.metamac.common.test.mock.ConfigurationServiceMockImpl;

public class ExternalItemsDto2DoMapperTest {

    @Rule
    public ExpectedException       thrown               = ExpectedException.none();

    private Dto2DoMapper           dto2DoMapper         = new Dto2DoMapperImpl();

    protected ConfigurationService configurationService = new ConfigurationServiceMockImpl();
    ExternalItemRepository         repository           = Mockito.mock(ExternalItemRepository.class);

    private static final String    URN_01               = "lorem:ipsum:externalItem:mock:01";
    private static final String    URN_02               = "lorem:ipsum:externalItem:mock:02";
    private static final String    METADATA_NAME        = "LOREM_IPSUM";

    @Before
    public void setConfigurationToMapper() throws Exception {
        setFieldToBaseMapper("configurationService", configurationService);
        setFieldToMapper("externalItemRepository", repository);
    }

    @After
    public void validateMockitoUsage() {
        Mockito.validateMockitoUsage();
    }

    @Test
    public void testExternalItemDtoToEntityNullDtoAndNullDo() throws Exception {
        // NULL, NULL
        testExternalItemDtoToEntity(null, null);
    }

    @Test
    public void testExternalItemDtoToEntityExistsDtoAndNullDo() throws Exception {
        // EXISTS, NULL
        ExternalItemDto externalItemDto = StatisticalOperationsMocks.mockExternalItemDtoComplete(URN_01, TypeExternalArtefactsEnum.AGENCY);
        testExternalItemDtoToEntity(externalItemDto, null);
    }

    @Test
    public void testExternalItemDtoToEntityNullDtoAndExistsDo() throws Exception {
        // NULL, EXISTS
        ExternalItem externalItem = StatisticalOperationsMocks.mockAgencyExternalItem();
        testExternalItemDtoToEntity(null, externalItem);
        Mockito.verify(repository).delete(Mockito.any(ExternalItem.class));
    }

    @Test
    public void testExternalItemDtoToEntityExistsDtoAndExistsDo() throws Exception {
        // EXISTS, EXISTS
        ExternalItemDto externalItemDto = MetamacMocks.mockExternalItemDtoComplete(URN_01, TypeExternalArtefactsEnum.AGENCY);
        ExternalItem externalItem = StatisticalOperationsMocks.mockAgencyExternalItem();
        testExternalItemDtoToEntity(externalItemDto, externalItem);
    }

    @Test
    public void testExternalItemListToEntityEmptyDtoAndEmptyDo() throws Exception {
        // EMPTY, EMPTY
        Set<ExternalItemDto> dtos = new HashSet<ExternalItemDto>();
        Set<ExternalItem> entities = new HashSet<ExternalItem>();
        testExternalItemDtoListToEntitiesList(dtos, entities);
    }

    @Test
    public void testExternalItemListToEntityExistsDtoAndEmptyDo() throws Exception {
        // EXISTS, EMPTY
        Set<ExternalItemDto> dtos = new HashSet<ExternalItemDto>();
        dtos.add(StatisticalOperationsMocks.mockExternalItemDtoComplete(URN_01, TypeExternalArtefactsEnum.AGENCY));
        dtos.add(StatisticalOperationsMocks.mockExternalItemDtoComplete(URN_02, TypeExternalArtefactsEnum.AGENCY));
        Set<ExternalItem> entities = new HashSet<ExternalItem>();

        testExternalItemDtoListToEntitiesList(dtos, entities);
    }

    @Test
    public void testExternalItemListToEntityEmptyDtoAndExistsDo() throws Exception {
        // EMPTY, EXISTS
        Set<ExternalItemDto> dtos = new HashSet<ExternalItemDto>();
        Set<ExternalItem> entities = new HashSet<ExternalItem>();
        entities.add(StatisticalOperationsMocks.mockAgencyExternalItem());
        entities.add(StatisticalOperationsMocks.mockAgencyExternalItem());

        testExternalItemDtoListToEntitiesList(dtos, entities);

        Mockito.verify(repository, times(2)).delete(Mockito.any(ExternalItem.class));
    }

    @Test
    public void testExternalItemListToEntityExistsDtoAndExistsDo() throws Exception {
        // EXISTS, EXISTS: Same elements
        Set<ExternalItemDto> dtos = new HashSet<ExternalItemDto>();
        dtos.add(new ExternalItemDto("CODE_01", ConfigurationMockConstants.SRM_INTERNAL_API_URL_BASE + CoreCommonConstants.URL_SEPARATOR + "URI_01", "URN_01", TypeExternalArtefactsEnum.AGENCY));
        dtos.add(new ExternalItemDto("CODE_02", ConfigurationMockConstants.SRM_INTERNAL_API_URL_BASE + CoreCommonConstants.URL_SEPARATOR + "URI_02", "URN_02", TypeExternalArtefactsEnum.CATEGORY));
        Set<ExternalItem> entities = new HashSet<ExternalItem>();
        entities.add(new ExternalItem("CODE_01", ConfigurationMockConstants.SRM_INTERNAL_API_URL_BASE + CoreCommonConstants.URL_SEPARATOR + "URI_01", "URN_01", TypeExternalArtefactsEnum.AGENCY));
        entities.add(new ExternalItem("CODE_02", ConfigurationMockConstants.SRM_INTERNAL_API_URL_BASE + CoreCommonConstants.URL_SEPARATOR + "URI_02", "URN_02", TypeExternalArtefactsEnum.CATEGORY));

        testExternalItemDtoListToEntitiesList(dtos, entities);
    }

    @Test
    public void testExternalItemListToEntityExistsDtoAndExistsDoWithDtoMoreElements() throws Exception {
        // EXISTS, EXISTS: More elements
        Set<ExternalItemDto> dtos = new HashSet<ExternalItemDto>();
        dtos.add(new ExternalItemDto("CODE_01", ConfigurationMockConstants.SRM_INTERNAL_API_URL_BASE + CoreCommonConstants.URL_SEPARATOR + "URI_01", "URN_01", TypeExternalArtefactsEnum.AGENCY));
        dtos.add(new ExternalItemDto("CODE_02", ConfigurationMockConstants.SRM_INTERNAL_API_URL_BASE + CoreCommonConstants.URL_SEPARATOR + "URI_02", "URN_02", TypeExternalArtefactsEnum.CATEGORY));
        dtos.add(new ExternalItemDto("CODE_03", ConfigurationMockConstants.SRM_INTERNAL_API_URL_BASE + CoreCommonConstants.URL_SEPARATOR + "URI_03", "URN_03", TypeExternalArtefactsEnum.CATEGORY));
        Set<ExternalItem> entities = new HashSet<ExternalItem>();
        entities.add(new ExternalItem("CODE_01", ConfigurationMockConstants.SRM_INTERNAL_API_URL_BASE + CoreCommonConstants.URL_SEPARATOR + "URI_01", "URN_01", TypeExternalArtefactsEnum.AGENCY));
        entities.add(new ExternalItem("CODE_02", ConfigurationMockConstants.SRM_INTERNAL_API_URL_BASE + CoreCommonConstants.URL_SEPARATOR + "URI_02", "URN_02", TypeExternalArtefactsEnum.CATEGORY));

        testExternalItemDtoListToEntitiesList(dtos, entities);
    }

    @Test
    public void testExternalItemListToEntityExistsDtoAndExistsDoWithDtoLessElements() throws Exception {
        // EXISTS, EXISTS: Less elements
        Set<ExternalItemDto> dtos = new HashSet<ExternalItemDto>();
        dtos.add(new ExternalItemDto("CODE_01", ConfigurationMockConstants.SRM_INTERNAL_API_URL_BASE + CoreCommonConstants.URL_SEPARATOR + "URI_01", "URN_01", TypeExternalArtefactsEnum.AGENCY));
        dtos.add(new ExternalItemDto("CODE_02", ConfigurationMockConstants.SRM_INTERNAL_API_URL_BASE + CoreCommonConstants.URL_SEPARATOR + "URI_02", "URN_02", TypeExternalArtefactsEnum.CATEGORY));
        Set<ExternalItem> entities = new HashSet<ExternalItem>();
        entities.add(new ExternalItem("CODE_01", "URI_01", "URN_01", TypeExternalArtefactsEnum.AGENCY));
        entities.add(new ExternalItem("CODE_02", "URI_02", "URN_02", TypeExternalArtefactsEnum.CATEGORY));
        entities.add(new ExternalItem("CODE_03", "URI_03", "URN_03", TypeExternalArtefactsEnum.CATEGORY));

        testExternalItemDtoListToEntitiesList(dtos, entities);

        Mockito.verify(repository).delete(Mockito.any(ExternalItem.class));
    }

    @Test
    public void testExternalItemListToEntityExistsDtoAndExistsDoWithDtoDifferentElements() throws Exception {
        // EXISTS, EXISTS: Different elements
        Set<ExternalItemDto> dtos = new HashSet<ExternalItemDto>();
        dtos.add(StatisticalOperationsMocks.mockExternalItemDtoComplete(URN_01, TypeExternalArtefactsEnum.AGENCY));
        dtos.add(StatisticalOperationsMocks.mockExternalItemDtoComplete(URN_02, TypeExternalArtefactsEnum.AGENCY));
        Set<ExternalItem> entities = new HashSet<ExternalItem>();
        entities.add(StatisticalOperationsMocks.mockAgencyExternalItem());
        entities.add(StatisticalOperationsMocks.mockAgencyExternalItem());

        testExternalItemDtoListToEntitiesList(dtos, entities);

        Mockito.verify(repository, times(2)).delete(Mockito.any(ExternalItem.class));
    }

    @Test
    public void testExternalItemListToEntityErrorExpectedAndActualNull() throws Exception {
        thrown.expect(InvocationTargetException.class);
        testExternalItemDtoListToEntitiesList(null, null);
    }

    @Test
    public void testExternalItemListToEntityErrorExpectedNull() throws Exception {
        thrown.expect(InvocationTargetException.class);
        Set<ExternalItem> entities = new HashSet<ExternalItem>();
        testExternalItemDtoListToEntitiesList(null, entities);
    }

    @Test
    public void testExternalItemListToEntityErrorActualNull() throws Exception {
        thrown.expect(InvocationTargetException.class);
        Set<ExternalItemDto> dtos = new HashSet<ExternalItemDto>();
        testExternalItemDtoListToEntitiesList(dtos, null);
    }

    private void setFieldToBaseMapper(String fieldName, ConfigurationService fieldValue) throws Exception {
        Field field = dto2DoMapper.getClass().getSuperclass().getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(dto2DoMapper, fieldValue);
    }
    
    private void setFieldToMapper(String fieldName, Object fieldValue) throws Exception {
        Field field = dto2DoMapper.getClass().getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(dto2DoMapper, fieldValue);
    }

    @SuppressWarnings("rawtypes")
    private Method getVisibleExternalItemDtoToEntityMethod() throws Exception {
        Class[] parameterTypes = new Class[3];
        parameterTypes[0] = ExternalItemDto.class;
        parameterTypes[1] = ExternalItem.class;
        parameterTypes[2] = String.class;
        Method externalItemDtoToEntityMethod = dto2DoMapper.getClass().getDeclaredMethod("externalItemDtoToEntity", parameterTypes);
        externalItemDtoToEntityMethod.setAccessible(true);
        return externalItemDtoToEntityMethod;
    }

    private void testExternalItemDtoToEntity(ExternalItemDto externalItemDto, ExternalItem externalItem) throws Exception {
        Method externalItemDtoToEntityMethod = getVisibleExternalItemDtoToEntityMethod();

        Object[] parameters = new Object[3];
        parameters[0] = externalItemDto;
        parameters[1] = externalItem;
        parameters[2] = METADATA_NAME;

        ExternalItem result = (ExternalItem) externalItemDtoToEntityMethod.invoke(dto2DoMapper, parameters);
        StatisticalOperationsAsserts.assertEqualsExternalItem(result, externalItemDto, MapperEnum.DTO2DO);
    }

    @SuppressWarnings("rawtypes")
    private Method getVisibleExternalItemListToEntityMethod() throws Exception {
        Class[] parameterTypes = new Class[3];
        parameterTypes[0] = Set.class;
        parameterTypes[1] = Set.class;
        parameterTypes[2] = String.class;
        Method externalItemListToEntityMethod = dto2DoMapper.getClass().getDeclaredMethod("externalItemListToEntity", parameterTypes);
        externalItemListToEntityMethod.setAccessible(true);
        return externalItemListToEntityMethod;
    }

    @SuppressWarnings("unchecked")
    private void testExternalItemDtoListToEntitiesList(Set<ExternalItemDto> dtos, Set<ExternalItem> entities) throws Exception {
        Method externalItemListToEntityMethod = getVisibleExternalItemListToEntityMethod();

        Object[] parameters = new Object[3];
        parameters[0] = dtos;
        parameters[1] = entities;
        parameters[2] = METADATA_NAME;

        Set<ExternalItem> result = (Set<ExternalItem>) externalItemListToEntityMethod.invoke(dto2DoMapper, parameters);
        StatisticalOperationsAsserts.assertEqualsExternalItemCollectionMapper(result, dtos, MapperEnum.DTO2DO);

    }
}
